/*
//
// Created by Дмитрий Скроба on 26.05.2021.
//

#include <iostream>
#include <vector>
#include <complex>

using namespace std;

#define long long long
# define M_PI           3.14159265358979323846

typedef complex<double> complex_num;

int n;
int arrSize = 1;
vector<int> first;
vector<int> second;
vector<int> ans;

void fft (vector<complex_num> & array, bool is_invert) {
    int arrSize = (int) array.size();
    if (arrSize == 1) {
        return;
    }

    vector<complex_num> left(arrSize / 2);
    vector<complex_num> right(arrSize / 2);

    for (int i = 0, j = 0; i < arrSize; i += 2, ++j) {
        left[j] = array[i];
        right[j] = array[i + 1];
    }

    fft (left, is_invert);
    fft (right, is_invert);

    double ang = 2 * M_PI / arrSize * (is_invert ? -1 : 1);

    complex_num w (1);
    complex_num wn (cos(ang), sin(ang));

    for (int i = 0; i < arrSize / 2; ++i) {
        array[i] = left[i] + w * right[i];
        array[i + arrSize / 2] = left[i] - w * right[i];
        if (is_invert) {
            array[i] /= 2;
            array[i + arrSize / 2] /= 2;
        }
        w *= wn;
    }
}

void multiply(const vector<int> & left, const vector<int> & right) {
    vector<complex_num> nLeft (left.begin(), left.end());
    vector<complex_num> nRight (right.begin(), right.end());

    fft(nLeft, false);
    fft (nRight, false);

    for (int i = 0; i < arrSize; ++i) {
        nLeft[i] *= nRight[i];
    }

    fft (nLeft, true);

    ans.resize(arrSize);

    for (int i = 0; i < arrSize; ++i) {
        ans[i] = int (nLeft[i].real() + 0.5);
    }
}

void compute() {
    multiply(first, second);

    int length =  arrSize - 1;

    while (ans[length] == 0) {
        length--;
    }

    n = length + 1;
}

void read() {
    cin >> n;

    while (arrSize < 2 * n + 1) {
        arrSize *= 2;
    }

    first.resize(arrSize, 0);
    second.resize(arrSize, 0);

    for (int i = 0; i < n + 1; ++i) {
        cin >> first[i];
    }

    for (int i = 0; i < n + 1; ++i) {
        cin >> second[i];
    }
}

void output() {
    for (int i = 0; i < n; ++i) {
        cout << ans[i] << " ";
    }
}

*/
/*int main() {
    read();
    compute();
    output();
    return 0;
}*//*


*/
