#include <iostream>
#include <vector>
#include <queue>
#include <set>

using namespace std;

typedef long long ll;

const ll MAX = 1e18;
const int LEN = 100000;
vector<pair<int, int>> graph[LEN];
int n;

vector<ll> dijkstra(int s)
{
    vector <ll> dp(n, MAX);
    set<pair<int, int>> queue;
    queue.insert({0, s});
    dp[s] = 0;
    while (!queue.empty())
    {
        int vertex = (*queue.begin()).second;
        ll weight = (*queue.begin()).first;

        queue.erase(queue.begin());
        for (int i = 0; i < graph[vertex].size(); i++)
        {
            pair<int, int> edge = graph[vertex][i];
            ll nleng = dp[vertex] + edge.second;
            if (nleng < dp[edge.first]) {
                queue.erase({dp[edge.first], edge.first});
                dp[edge.first] = nleng;
                queue.insert({dp[edge.first], edge.first});
            }
        }
    }
    return dp;
}

int main()
{
    int m;
    cin >> n >> m;
    for (int i = 0; i < m; i++)
    {
        int from, to, weight;
        cin >> from >> to >> weight;
        from--;
        to--;
        graph[from].push_back({to, weight});
        graph[to].push_back({from, weight});
    }
    int a, b, c;
    cin >> a >> b >> c;
    a--;
    b--;
    c--;
    vector<ll> distB = dijkstra(b);
    if (distB[a] == MAX || distB[c] == MAX) {
        cout << "-1";
    } else {
        vector<ll> distA = dijkstra(a);
        if (distA[c] ==  MAX) {
            cout << "-1";
        } else {
            ll abc = distA[b] + distB[c];
            ll acb = distA[c] + distB[c];
            ll bac = distB[a] + distA[c];
            cout << min(abc, min(acb, bac));
        }
    }
    return 0;
}
