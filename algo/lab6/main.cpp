#include <iostream>
#include <vector>
#include <complex>
#include <string>

using namespace std;

#define long long long
# define M_PI           3.14159265358979323846

typedef complex<double> complex_num;

int n;
int arrSize = 1;
long answer = 0;
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
        ans[i] = (int) round(nLeft[i].real());
    }
}

void compute() {
    multiply(first, second);

    for (int i = 0; i < first.size(); i++) {
        if (first[i] == 1) {
            answer += (ans[2 * i] - 1) / 2;
        }
    }
}

void read() {
    string input;

    cin >> input;

    n = input.length();

    while (arrSize < 2 * n + 1) {
        arrSize *= 2;
    }

    first.resize(arrSize, 0);
    second.resize(arrSize, 0);

    for (int i = 0; i < input.length(); ++i) {
        char ch = input[i];
        first[i] = ch == '0' ? 0 : 1;
        second[i] = ch == '0' ? 0 : 1;
    }
}

void output() {
    cout << answer;
}

int main() {
    read();
    compute();
    output();
    return 0;
}
