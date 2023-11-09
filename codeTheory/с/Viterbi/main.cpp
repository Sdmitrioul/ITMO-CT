#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include <string>
#include <algorithm>
#include <chrono>
#include <random>

class TrellisNode;

using namespace std;
using uint = unsigned int;
using Vec = vector<bool>;
using Matrix = vector<Vec>;
using Spins = vector<pair<uint, uint>>;
using Signal = vector<double>;
using TrellisGraph = vector<vector<TrellisNode *>>;

//Generate next value.
double next_gaussian(double mean, double sigma) {
    static default_random_engine generator(chrono::system_clock::now().time_since_epoch().count());

    normal_distribution<double> normalDistribution(mean, sigma);

    return normalDistribution(generator);
}

//Node of trellis graph.
class TrellisNode {
public:
    //Next nodes by (0, 1) paths.
    TrellisNode *zero = nullptr;
    TrellisNode *one = nullptr;

    //For building trellis graph.
    vector<pair<bool, int>> code;

    //For saving paths.
    double distance = -1;
    TrellisNode *prev = nullptr;
};

//Class for decoder.
class Decoder {
public:
    Decoder(const unsigned int &count_of_rows, const unsigned int &count_of_columns, const Matrix &matrix) {
        this->count_of_rows = count_of_rows;
        this->count_of_columns = count_of_columns;
        this->matrix = matrix;
        //Build trellis graph: msf matrix -> spins -> build graph.
        init();
    }

    //Encoding bit vector.
    Vec encode(const Vec &input) {
        Vec result = Vec(this->count_of_columns, false);

        for (uint i = 0; i < this->count_of_columns; i++) {
            bool accum = false;

            for (uint j = 0; j < this->count_of_rows; j++) {
                accum ^= this->matrix[j][i] & input[j];
            }

            result[i] = accum;
        }

        return result;
    }

    //Decoding vector of signals.
    Vec decode(const Signal &signal) {
        //Set distance from start node to 0;
        this->graph_layers[0][0]->distance = 0;

        //Set distance in other nodes(not starting) to default value(-1)
        for (int i = 1; i < this->graph_layers.size(); ++i) {
            for (auto *node: this->graph_layers[i]) {
                node->distance = DEFAULT_DISTANCE_VALUE;
                node->prev = nullptr;
            }
        }

        //Calculate distance to each node of graph to find min way using dp.
        for (int i = 0; i < signal.size(); ++i) {
            const double value = signal[i];

            for (auto *node: this->graph_layers[i]) {
                if (node->zero != nullptr) {
                    Decoder::updateDistance(node, node->zero, -value);
                }

                if (node->one != nullptr) {
                    Decoder::updateDistance(node, node->one, value);
                }
            }
        }

        Vec answer;

        //Get the final node.
        TrellisNode *current = this->graph_layers[this->graph_layers.size() - 1][0];

        //Get path to final node.
        for (uint layer = this->graph_layers.size() - 1; layer >= 1 && current; layer--) {
            TrellisNode *prev = current->prev;

            if (prev && prev->zero != nullptr && prev->zero == current) {
                answer.push_back(false);
            } else {
                answer.push_back(true);
            }

            current = prev;
        }

        //Revert path.
        reverse(answer.begin(), answer.end());

        return answer;
    }

    //Simulate sending vector.
    double simulate(const double &noise, const uint &max_iterations, const uint &max_errors) {
        uint mistakes = 0;
        uint iterations = 0;

        Vec vec(this->count_of_columns);

        for (uint i = 0; i < max_iterations; i++) {
            iterations++;

            //Fill vector with random bits.
            Decoder::random_fill(vec);

            //Encode vector.
            Vec encoded = this->encode(vec);

            //Add noise to encoded vector.
            Signal signal = this->send_signal(encoded, noise);

            //Decoding of vector with noises&
            Vec decoded = this->decode(signal);

            //Compare sended vector and vector we got after decoding.
            if (decoded != encoded) {
                mistakes++;
            }

            if (mistakes == max_errors) {
                break;
            }
        }

        return double(mistakes) / iterations;
    }

    //Return vector of sizes of each layer of trellis graph.
    vector<uint> sizes() {
        const uint size = this->graph_layers.size();

        vector<uint> result(size, 0);

        for (int i = 0; i < size; ++i) {
            result[i] = this->graph_layers[i].size();
        }

        return result;
    }

private:
    static const int DEFAULT_DISTANCE_VALUE = -1;

    uint count_of_rows;
    uint count_of_columns;

    Matrix matrix;
    TrellisGraph graph_layers;

    //Helper function for updating distance to node.
    static void updateDistance(TrellisNode *current, TrellisNode *next, const double &distance) {
        if (next->distance == DEFAULT_DISTANCE_VALUE || next->distance > current->distance + distance) {
            next->distance = current->distance + distance;
            next->prev = current;
        }
    }

    //Helper function for filling vector with random bits.
    static void random_fill(Vec &vec) {
        mt19937 get_random((random_device()) ());

        for (auto &&i: vec) {
            i = get_random() % 2;
        }
    }

    //Helper function for noising vector with given noise.
    Signal send_signal(Vec &vec, const double &noise) const {
        Signal result(vec.size());

        double decibel = pow(10, -noise / 10);
        double deviation = (0.5 * this->count_of_columns / this->count_of_rows) * decibel;
        double sigma = sqrt(deviation);

        for (int i = 0; i < result.size(); ++i) {
            double bit = vec[i] ? -1.0 : 1.0;
            result[i] = bit + next_gaussian(0.0, sigma);
        }

        return result;
    }

    //Function for building Trellis graph.
    void init() {
        //Convert matrix to MSF form.
        Matrix msf_matrix = getMinimalSpanFormMatrix();

        //Get spins from MSF matrix.
        Spins spins = get_spins(msf_matrix);

        const uint rows_count = this->count_of_rows;
        const uint column_count = this->count_of_columns;

        auto *start = new TrellisNode();

        TrellisGraph layers(column_count + 1);
        vector<TrellisNode *> prev_layer;
        vector<uint> count_of_active;

        prev_layer.push_back(start);
        layers[0].push_back(start);

        for (uint i = 1; i < column_count + 1; i++) {
            uint matrix_column = i - 1;

            vector<bool> column(rows_count);

            //Get current column for layer.
            for (int j = 0; j < rows_count; ++j) {
                column[j] = msf_matrix[j][matrix_column];
            }

            int first_active_layer = -1;
            vector<uint> active_layers;

            //Get active raws.
            for (int j = 0; j < rows_count; ++j) {
                if (matrix_column == spins[j].first) {
                    first_active_layer = j;
                }
                if (matrix_column >= spins[j].first && matrix_column <= spins[j].second) {
                    active_layers.push_back(j);
                }
            }

            map<vector<pair<bool, int>>, TrellisNode *> next_layer;

            //Calculate new layer by prev nodes.
            for (TrellisNode *node: prev_layer) {
                //If we don't have new active layer, just making edges.
                if (first_active_layer == -1) {
                    vector<pair<bool, int>> n_node = node->code;

                    bool edge = mul(column, n_node);

                    Decoder::add_edge(n_node, spins, node, edge, next_layer, matrix_column);
                    continue;
                }

                //Add new active node.
                vector<pair<bool, int>> zero = node->code;
                vector<pair<bool, int>> one = node->code;

                zero.emplace_back(0, first_active_layer);
                one.emplace_back(1, first_active_layer);

                bool on_one = mul(column, one);

                Decoder::add_edge(zero, spins, node, !on_one, next_layer, matrix_column);
                Decoder::add_edge(one, spins, node, on_one, next_layer,  matrix_column);
            }

            prev_layer.clear();

            for (const pair<vector<pair<bool, int>>, TrellisNode *> cur: next_layer) {
                prev_layer.push_back(cur.second);
            }

            layers[i] = prev_layer;
        }

        this->graph_layers = layers;
    }

    //Get spins from MSF matrix!
    [[nodiscard]] Spins get_spins(const Matrix &msf_matrix) const {
        const uint rows_count = this->count_of_rows;
        const uint column_count = this->count_of_columns;

        Spins spins;

        for (int i = 0; i < rows_count; i++) {
            uint start = 0;
            uint end = column_count - 1;

            while (!msf_matrix[i][start]) {
                start++;
            }

            while (!msf_matrix[i][end]) {
                end--;
            }

            if (start < end) {
                end--;
            } else {
                end = start;
            }

            spins.emplace_back(start, end);
        }

        return spins;
    }

    //Gauss modification of matrix to make starts and ends of active nods unique.
    Matrix getMinimalSpanFormMatrix() {
        Matrix g_matrix = this->matrix;
        const uint rows_count = this->count_of_rows;

        uint column = 0;

        //Make begin of row unique.
        for (uint row_current = 0; row_current < rows_count; row_current++) {
            bool isClear = true;

            if (!g_matrix[row_current][column]) {
                isClear = false;

                for (uint row = row_current + 1; row < rows_count; row++) {
                    if (g_matrix[row][column]) {
                        xor_rows(row, row_current, g_matrix);
                        isClear = true;
                        break;
                    }
                }
            }

            if (isClear) {
                for (uint row = row_current + 1; row < rows_count; row++) {
                    if (g_matrix[row][column]) {
                        xor_rows(row_current, row, g_matrix);
                    }
                }
            } else {
                row_current--;
            }

            column++;
        }

        Vec free(rows_count, true);
        column = this->count_of_columns - 1;

        //Make end of row unique.
        for (uint step = 0; step < rows_count; step++) {
            int count = 0;

            for (uint i = 0; i < rows_count; i++) {
                if (free[i] && g_matrix[i][column]) {
                    count++;
                }
            }

            if (count > 0) {
                uint last = rows_count - 1;

                while (!(g_matrix[last][column] && free[last])) {
                    last--;
                }

                free[last] = false;

                if (count > 1) {
                    for (uint i = 0; i < last; i++) {
                        if (g_matrix[i][column] && free[i]) {
                            xor_rows(last, i, g_matrix);
                        }
                    }
                }
            } else {
                step--;
            }

            column--;
        }

        return g_matrix;
    }

    static TrellisNode *get_nodes_by_id(
            map<vector<pair<bool, int>>,
                    TrellisNode *> &gr,
            const vector<pair<bool, int>> &key
    ) {
        if (gr.count(key) != 0) {
            return gr[key];
        }

        auto *new_node = new TrellisNode();

        new_node->code = key;

        gr[key] = new_node;

        return new_node;
    }

    static void add_edge(
            vector<pair<bool, int>> &code,
            const Spins &spins, TrellisNode *node,
            bool positive, map<vector<pair<bool, int>>,
            TrellisNode *> &next_layer,
            const uint matrix_column
            ) {
        rm_position(code, spins, matrix_column);

        TrellisNode *new_node = get_nodes_by_id(next_layer, code);

        if (positive) {
            node->one = new_node;
        } else {
            node->zero = new_node;
        }
    }

    static void rm_position(vector<pair<bool, int>> &vec, const vector<pair<uint, uint>> &spins, const uint pos) {
        for (int i = 0; i < vec.size(); i++) {
            if (pos > spins[vec[i].second].second) {
                vec.erase(vec.begin() + i);
                break;
            }
        }
    }

    static bool mul(const Vec &prev_column, const vector<pair<bool, int>> &next_layer) {
        bool result = false;

        for (const pair<bool, int> &p: next_layer) {
            result ^= p.first && prev_column[p.second];
        }

        return result;
    }

    //Add first row tto second.
    void xor_rows(const uint &row1, const uint &row2, Matrix &m) const {
        for (uint i = 0; i < this->count_of_columns; i++) {
            m[row2][i] = m[row2][i] ^ m[row1][i];
        }
    }
};

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    uint count_of_columns, count_of_rows;

    cin >> count_of_columns >> count_of_rows;

    Matrix matrix(count_of_rows, Vec(count_of_columns, false));

    // Read matrix G
    for (int i = 0; i < count_of_rows; i++) {
        for (int j = 0; j < count_of_columns; j++) {
            uint val;
            cin >> val;
            matrix[i][j] = (val == 1);
        }
    }

    //Create decoder from matrix G
    Decoder decoder(count_of_rows, count_of_columns, matrix);

    //Get vector containing trellis graph(it is creating while creating decoder)
    vector<uint> levels = decoder.sizes();

    for (uint level: levels) {
        cout << level << " ";
    }

    string command;
    double noise_level;
    uint iterations, errors;
    Vec input = Vec(count_of_rows);
    Signal signal = Signal(count_of_columns);

    while (cin >> command) {
        cout << "\n";

        if (command == "Encode") {
            // Read vector for encoding
            for (int i = 0; i < count_of_rows; i++) {
                uint val;
                cin >> val;
                input[i] = val == 1;
            }

            Vec result = decoder.encode(input);

            for (int i = 0; i < count_of_columns; i++) {
                cout << result[i] << " ";
            }
        } else if (command == "Decode") {
            //Read signal vector!
            for (int i = 0; i < count_of_columns; ++i) {
                cin >> signal[i];
            }

            Vec result = decoder.decode(signal);

            for (int i = 0; i < count_of_columns; i++) {
                cout << result[i] << " ";
            }
        } else if (command == "Simulate") {
            cin >> noise_level >> iterations >> errors;

            cout << decoder.simulate(noise_level, iterations, errors);
        }
    }

    return 0;
}
