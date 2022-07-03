/*
//
// Created by Дмитрий Скроба on 06.12.2020.
//

#include <iostream>
#include <algorithm>
#include <deque>
#include <vector>

using namespace std;

deque<int> queue;
vector<vector<bool>> matrix;

void reverse(int from, int to) {
    for (int i = 0; i < (to - from + 1) / 2; i++) {
        swap(queue[to - i], queue[from + i]);
    }
}

void findHamiltonCycle(int size) {
    for (int i = 0; i <= size * (size - 1); i++) {
        if (!matrix[queue[0]][queue[1]]) {
            int j = 2;
            while (!matrix[queue[0]][queue[j]] || (*/
/*j + 1 < queue.size() &&*//*
 !matrix[queue[1]][queue[j + 1]])) {
                j++;
            }
            reverse(1, j);
        }
        queue.push_back(queue.front());
        queue.pop_front();
    }
}

int main() {
    int size;
    cin >> size;

    matrix.resize(size, vector<bool> (size, false));

    for (int i = 0; i < size; ++i) {
        queue.push_back(i);
    }

    for (int i = 0; i < size; i++) {
        string line;
        getline(cin, line);
        for (int j = 0; j < i; j++) {
            matrix[i][j] = matrix[j][i] = '1' == line[j];
        }
    }

    findHamiltonCycle(size);

    for (int i = 0; i < queue.size(); i++) {
        cout << queue[i] + 1 << ' ';
    }
    return 0;
}
*/










/*
//
// Created by Дмитрий Скроба on 06.12.2020.
//

#include <iostream>
#include <algorithm>
#include <deque>
#include <vector>

using namespace std;

deque<int> queue;
vector<vector<bool>> matrix;

void reverse(int from, int to) {
    for (int i = 0; i < (to - from + 1) / 2; i++) {
        swap(queue[to - i], queue[from + i]);
    }
}

void findHamiltonCycle(int size) {
    for (int i = 0; i <= size * (size - 1); i++) {
        if (!matrix[queue[0]][queue[1]]) {
            int j = 2;
            while (!matrix[queue[0]][queue[j]] || (*/
/*j + 1 < queue.size() &&*//*
 !matrix[queue[1]][queue[j + 1]])) {
                j++;
            }
            reverse(1, j);
        }
        queue.push_back(queue.front());
        queue.pop_front();
    }
}

int main() {
    int size;
    cin >> size;

    matrix.resize(size, vector<bool> (size, false));

    for (int i = 0; i < size; ++i) {
        queue.push_back(i);
    }

    for (int i = 0; i < size; i++) {
        string line;
        getline(cin, line);
        for (int j = 0; j < i; j++) {
            matrix[i][j] = matrix[j][i] = '1' == line[j];
        }
    }

    findHamiltonCycle(size);

    for (int i = 0; i < queue.size(); i++) {
        cout << queue[i] + 1 << ' ';
    }
    return 0;
}
*/
