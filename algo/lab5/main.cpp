#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <climits>

using namespace std;

#define long long long

struct edge
{
    int from;
    int to;
    int flow;
    int weight;
    long cost;
    edge(int from, int to, int weight, long cost) : from(from), to(to), flow(0), weight(weight), cost(cost) {};
};

long INF = 1e18 + 9;
int n, m, from, to;
long answer = 0;
vector<long> cities;
vector<vector<pair<int, long>>> input_graph;
vector<vector<int>> graph;
vector<edge> edges;

void add_edge(int f, int t, int weight, long cost)
{
    int index = (int) edges.size();
    edges.emplace_back(f, t, weight, cost);
    edges.emplace_back(t, f, 0, -cost);
    graph[f].push_back(index);
    graph[t].push_back(index + 1);
}

void preprocess()
{
    graph.resize(2 * n + 2);

    for (int i = 0; i < n; i++) {
        vector<long> d (n, INF);
        vector<int> p(n);
        d[i] = 0;
        vector<bool> used(n, false);
        for (int j = 0; j < n; j++) {
            int v = -1;
            for (int l = 0; l < n; l++) {
                if (!used[l] && (v == -1 || d[l] < d[v])) {
                    v = l;
                }
            }
            if (d[v] == INF) {
                break;
            }
            used[v] = true;
            for (auto pair : input_graph[v]) {
                if (d[v] + pair.second < d[pair.first]) {
                    d[pair.first] = d[v] + pair.second;
                    p[pair.first] = v;
                }
            }
        }

        for (int j = 0; j < n; j++) {
            if (d[j] != INF && i != j) {
                add_edge(i, j + n, 1, d[j]);
            }
        }
    }

    from = 2 * n;
    to = 2 * n + 1;

    for (int i = 0; i < n; i++) {
        add_edge(from, i, 1, 0);
        add_edge(i + n, to, 1, 0);
        add_edge(i, n + i, 1, cities[i]);
    }
}

bool path_exist(vector<int> &parents)
{
    vector<long> dist(graph.size(), INF);
    dist[from] = 0;
    set<int> queue;
    queue.insert(from);

    while (!queue.empty())
    {
        int vertex = *(queue.begin());
        queue.erase(vertex);
        for (int t : graph[vertex]) {
            auto edge = edges[t];
            if (dist[edge.to] <= dist[edge.from] + edge.cost || edge.weight - edge.flow == 0) {
                continue;
            }
            queue.erase(edge.to);
            dist[edge.to] = dist[edge.from] + edge.cost;
            parents[edge.to] = t;
            queue.insert(edge.to);
        }
    }

    return dist[to] != INF;
}

int get_flow(int f, int t, vector<int> &parents)
{
    if (f == t)
    {
        return INT_MAX;
    }

    int flow = edges[parents[t]].flow;
    int weight = edges[parents[t]].weight;
    int fr = edges[parents[t]].from;

    return min(weight - flow, get_flow(f, fr, parents));
}

void process()
{
    preprocess();

    vector<int> parents(graph.size(), -1);

    while (path_exist(parents))
    {
        int flow = get_flow(from, to, parents);

        for (int i = to; i != from; i = edges[parents[i]].from)
        {
            int pos = parents[i];
            edges[pos].flow += flow;
            edges[pos + (pos % 2 == 0 ? 1 : -1)].flow -= flow;
            answer += edges[pos].cost * flow;
        }
    }
}

void read()
{
    cin >> n >> m;

    cities.resize(n);
    input_graph.resize(n);

    for (int i = 0; i < n; ++i) {
        cin >> cities[i];
    }

    for (int j = 0; j < m; ++j) {
        int f, t;
        long cost;
        cin >> f >> t >> cost;
        f -= 1;
        t -= 1;
        input_graph[f].push_back(make_pair(t, cost));
    }
}

void output()
{
    cout << answer << "\n";
}

int main()
{
    read();
    process();
    output();
    return 0;
}
