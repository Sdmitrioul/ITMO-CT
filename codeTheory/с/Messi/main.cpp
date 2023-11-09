#pragma GCC optimize("O2")

#include <iostream>
#include <vector>
#include <random>
#include <set>

using namespace std;
using uint = unsigned int;
using Vec = vector<bool>;

class Decoder {
public:
    Decoder(int size, int mask, int distance) {
        this->size = size;
        this->mask = mask;
        this->distance = distance;

        this->init();
    }

    Vec encode(Vec &input) {
        Vec answer(this->r, false);

        for (auto &&i: input) {
            answer.push_back(i);
        }

        Vec copy = answer;

        for (int i = answer.size() - 1; i >= this->r; i--) {
            if (copy[i]) {
                for (int j = 0; j < this->polynomic.size(); j++) {
                    copy[i - j] = copy[i - j] ^ this->polynomic[this->r - j];
                }
            }
        }

        for (int i = 0; i < this->r; i++) {
            answer[i] = copy[i];
        }

        return answer;
    }

    Vec decode(Vec &input) {
        Vec answer = input;

        vector<int> syndrome;
        int counter = 0;

        for (int j = 1; j < distance; j++) {
            int t = 0;

            for (int i = 0; i < answer.size(); i++) {
                if (answer[i]) {
                    t ^= alpha[(i * j) % size];
                }
            }

            if (t == 0) {
                counter++;
            }

            syndrome.push_back(t);
        }

        if (counter == distance - 1) {
            return answer;
        }

        int register_length = 0;
        int m = 0;
        vector<int> lambda(1, 1);
        vector<int> compensation(1, 1);

        for (int index = 1; index < distance; index++) {
            int delta = 0;
            int length = min(register_length, (int) lambda.size()) + 1;

            for (int j = 0; j < length; j++) {
                delta ^= dp[lambda[j]][syndrome[index - j - 1]];
            }

            if (delta != 0) {
                vector<int> copy = lambda;

                copy.resize(max(copy.size(), index - m + compensation.size()), 0);

                for (int i = 0; i < compensation.size(); i++) {
                    copy[index - m + i] ^= dp[delta][compensation[i]];
                }

                if (2 * register_length < index) {
                    compensation.clear();

                    int dd = alpha[(size - ind_alpha[delta]) % size];

                    for (int i: lambda) {
                        compensation.push_back(dp[dd][i]);
                    }

                    register_length = index - register_length;
                    m = index;
                }

                lambda = copy;
            }
        }

        if (register_length != lambda.size() - 1) {
            return answer;
        }

        vector<int> err;

        for (int i = 0; i < this->size; i++) {
            int res = lambda[0];

            for (int j = 1; j < lambda.size(); j++) {
                res ^= dp[alpha[(j * i) % size]][lambda[j]];
            }

            if (res == 0) {
                err.push_back((size - i) % size);

                if (err.size() == register_length) {
                    break;
                }
            }
        }

        for (int e: err) {
            answer[e] = !answer[e];
        }

        return answer;
     }

    double simulate(const double &noise, const uint &max_iterations, const uint &max_errors) {
        uint mistakes = 0;
        uint iterations = 0;
        Vec vec(this->k);

        for (uint i = 0; i < max_iterations; i++) {
            iterations++;

            //Fill vector with random bits.
            Decoder::random_fill(vec);

            //Encode vector.
            Vec encoded = this->encode(vec);

            //Add noise to encoded vector.
            Vec signal = this->send_signal(encoded, noise);

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

    int encoding_size() {
        return this->k;
    }

    Vec get_polynomic() {
        return this->polynomic;
    }

private:
    int size;
    int mask;
    int distance;

    int r{};
    int k{};
    vector<int> alpha;
    vector<int> ind_alpha;
    vector<vector<int>> dp;
    Vec polynomic;
    random_device rd{};
    mt19937 gen{rd()};

    void init() {
        int a = this->mask;
        int b = 1;

        while (a >>= 1) {
            b <<= 1;
        }

        this->alpha.resize(this->size);
        this->ind_alpha.resize(this->size + 1, -1);

        this->alpha[0] = 1;
        this->ind_alpha[1] = 0;

        for (int i = 1; i < this->size; ++i) {
            int val = this->alpha[i - 1] << 1;

            if (val >= b) {
                val ^= this->mask;
            }

            this->alpha[i] = val;
            this->ind_alpha[val] = i;
        }

        this->dp = vector<vector<int>>(this->size + 1, vector<int>(this->size + 1));

        for (int i = 1; i < this->size + 1; ++i) {
            for (int j = 1; j < this->size + 1; ++j) {
                this->dp[i][j] = this->alpha[(this->ind_alpha[i] + this->ind_alpha[j]) % this->size];
            }
        }

        for (int i = 0; i <= this->size; ++i) {
            this->dp[0][i] = 0;
        }

        set<int> all_values;

        for (int i = 1; i < this->size; ++i) {
            all_values.insert(i);
        }

        vector<set<int>> cyclotomics;

        while (!all_values.empty()) {
            int el = *all_values.begin();

            if (distance <= el) {
                break;
            }

            set<int> cur_class;

            //Check if set not contains el
            while (cur_class.find(el) == cur_class.end()) {
                cur_class.insert(el);
                all_values.erase(el);
                el = (2 * el) % this->size;
            }

            cyclotomics.push_back(cur_class);
        }

        Vec res;
        res.push_back(true);

        for (const auto &cur_cyclonomic: cyclotomics) {
            vector<int> min_pol;
            min_pol.push_back(0);

            for (auto element: cur_cyclonomic) {
                vector<int> pol_next;
                pol_next.push_back(-1);

                for (auto el: min_pol) {
                    pol_next.push_back(el);
                }

                for (int i = 0; i < min_pol.size(); i++) {
                    if (min_pol[i] >= 0) {
                        min_pol[i] = (min_pol[i] + element) % this->size;
                    }

                    if (min_pol[i] >= 0 && pol_next[i] >= 0) {
                        min_pol[i] = this->ind_alpha[this->alpha[min_pol[i]] ^ this->alpha[pol_next[i]]];
                    } else if (pol_next[i] >= 0) {
                        min_pol[i] = pol_next[i];
                    }
                }

                min_pol.push_back(pol_next[pol_next.size() - 1]);
            }

            Vec cur;

            for (auto el: min_pol) {
                cur.push_back(el == 0);
            }

            res = Decoder::mul(res, cur);
        }

        this->polynomic = res;
        this->r = (int) res.size() - 1;
        this->k = this->size - r;
    }

    Vec send_signal(Vec &encoded, const double &noise) {
        Vec result = encoded;

        uniform_real_distribution<> dist(0, 1);

        for (int i = 0; i < encoded.size(); ++i) {
            result[i] = dist(gen) > noise ? result[i] : !result[i];
        }

        return result;
    }

    inline static Vec mul(Vec &first, Vec &second) {
        Vec result(first.size() + second.size() - 1, false);

        for (int i = 0; i < first.size(); ++i) {
            for (int j = 0; j < second.size(); ++j) {
                result[i + j] = result[i + j] ^ (first[i] & second[j]);
            }
        }

        return result;
    }

    //Helper function for filling vector with random bits.
    static void random_fill(Vec &vec) {
        mt19937 get_random((random_device()) ());

        for (auto &&i: vec) {
            i = get_random() % 2;
        }
    }
};

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    int size, mask, dist;

    cin >> size >> mask >> dist;

    Decoder decoder(size, mask, dist);

    const uint encoding_size = decoder.encoding_size();

    string command;
    double noise_level;
    uint iterations, errors;
    Vec enc_input(encoding_size, false);
    Vec dec_input(size, false);

    cout << decoder.encoding_size() << "\n";

    Vec pol = decoder.get_polynomic();

    for (auto &&i: pol) {
        cout << i << " ";
    }

    while (cin >> command) {
        cout << "\n";

        if (command == "Encode") {
            for (uint i = 0; i < encoding_size; ++i) {
                uint val;
                cin >> val;
                enc_input[i] = val == 1;
            }

            Vec encoded = decoder.encode(enc_input);

            for (const bool el: encoded) {
                cout << el << " ";
            }
        } else if (command == "Decode") {
            for (uint i = 0; i < size; ++i) {
                uint val;
                cin >> val;
                dec_input[i] = val == 1;
            }

            Vec decoded = decoder.decode(dec_input);

            for (const bool el: decoded) {
                cout << el << " ";
            }
        } else if (command == "Simulate") {
            cin >> noise_level >> iterations >> errors;

            cout << decoder.simulate(noise_level, iterations, errors);
        }
    }

    return 0;
}