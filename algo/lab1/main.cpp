#include <iostream>
#include <vector>
#include <algorithm>
#include <queue>
#include <set>
#include <math.h>

using namespace std;

typedef int ll;

vector<vector<pair<int, int>>> E;
int N;
int M;

int main() {
    cin >> N >> M;
    E.resize(N);
    int x, y, weight;
    for (int i = 0; i < M; i++)
    {
        cin >> x >> y >> weight;
        E[x].push_back(make_pair(y, weight));
        E[y].push_back(make_pair(x, weight));
    }

    set<int> queue;
    queue.insert(0);

    vector<int> distanse(N, INT_MAX);
    distanse[0] = 0;

    while (!queue.empty())
    {
        int v = *(queue.begin());
        queue.erase(queue.begin());

        for (int j = 0; j < E[v].size(); j++) {
            pair<int, int> edge = E[v][j];
            if (distanse[v] + edge.second < distanse[edge.first]) {
                distanse[edge.first] = distanse[v] + edge.second;
                queue.insert(edge.first);
            }
        }
    }

    for (int i : distanse) {
        cout << i << ' ';
    }
    return 0;
}
