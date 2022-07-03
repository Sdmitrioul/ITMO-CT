#include <iostream>
#include <vector>
#include <queue>

using namespace std;

void graphAnalysis(int n, vector<int> *graph, int *out)
{
    size_t size = n;
    int ans[size];
    int outLocal[size];
    queue<int> queue;
    for (size_t i = 0; i < size; i++)
    {
        ans[i] = 0;
        outLocal[i] = 0;
        if (out[i] == 0)
        {
            ans[i] = -1;
            queue.push(i);
        }
    }
    while (!queue.empty())
    {
        int vertex = queue.front();
        queue.pop();
        if (ans[vertex] == -1)
        {
            for (int u : graph[vertex])
            {
                if (ans[u] == 0)
                {
                    ans[u] = 1;
                    queue.push(u);
                }
            }
        } else {
            for (int u : graph[vertex])
            {
                outLocal[u]++;
                if (outLocal[u] == out[u])
                {
                    ans[u] = -1;
                    queue.push(u);
                }
            }
        }
    }
    for (int i : ans)
    {
        if (i == 0)
        {
            cout << "DRAW\n";
        } else if (i == -1)
        {
            cout << "SECOND\n";
        } else
        {
            cout << "FIRST\n";
        }
    }
}

/*
int main()
{
    int n, m;
    while (cin >> n >> m)
    {
        int from, to;
        vector<int> graph[n];
        int out[n];
        for (int i = 0; i < n; i++)
        {
            out[i] = 0;
        }
        for (int i = 0; i < m; i++) {
            cin >> from >> to;
            from--;
            to--;
            graph[to].push_back(from);
            out[from]++;
        }
        graphAnalysis(n, graph, out);
        cout << "\n";
    }
    cout << endl;
    return 0;
}
*/
