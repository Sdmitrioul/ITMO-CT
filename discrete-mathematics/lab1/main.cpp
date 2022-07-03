//
// Created by Дмитрий Скроба on 06.12.2020.
//
#include <iostream>
#include <queue>
#include <vector>
#include <set>

using namespace std;

int main() {
    int vertexSize, edgeSize;
    cin >> vertexSize >> edgeSize;

    vector<vector<int>> matrix(vertexSize);

    for (int i = 0; i < edgeSize; ++i) {
        int from, to;
        cin >> from >> to;
        from--;
        to--;
        matrix[from].push_back(to);
        matrix[to].push_back(from);
    }

    int maxi = 0;

    vector<int> color(vertexSize, -1);
    queue<int> queue;

    queue.push(0);
    while (!queue.empty()) {
        int vertex = queue.front();
        queue.pop();
        set<int> set;
        for (int j = 0; j < matrix[vertex].size(); j++) {
            int i = matrix[vertex][j];
            if (color[i] == -1) {
                queue.push(i);
            } else {
                set.insert(color[i]);
            }
        }
        int i = 0;
        while (set.count(i)) {
            i++;
        }
        color[vertex] = i;
        maxi = max(maxi, i);
    }

    maxi++;

    cout << ((maxi % 2 == 0) ? maxi + 1 : maxi);
    cout << "\n";
    for (int i = 0; i < color.size(); i++) {
        cout << color[i] + 1;
        cout << "\n";
    }
    return 0;
}