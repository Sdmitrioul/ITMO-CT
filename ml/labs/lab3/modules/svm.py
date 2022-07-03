import numpy as np
from numpy import ndarray


def svm(x: ndarray, y: ndarray, c, kernel, eps: float = 1e-8) -> (float, ndarray):
    omega = 0
    size = len(y)

    omegas = np.zeros(size)
    kernels = np.array([[kernel(x[i], x[q]) for q in range(size)] for i in range(size)])

    for _ in range(150):
        for i in range(size):
            error_i = omega + np.sum(y * omegas * kernels[i]) - y[i]

            q = np.random.randint(0, size)

            while i == q:
                q = np.random.randint(0, size)

            error_q = omega + np.sum(y * omegas * kernels[q]) - y[q]

            if (y[i] * error_i > eps and omegas[i] > 0) or (y[i] * error_i < -eps and omegas[i] < c):
                save_omega_i = omegas[i]
                save_omega_q = omegas[q]

                l = max(0, omegas[i] + omegas[q] - c)
                r = min(c, omegas[i] + omegas[q])

                if y[i] != y[q]:
                    l = max(0, omegas[q] - omegas[i])
                    r = min(c, c + omegas[q] - omegas[i])

                derivative = 2 * kernels[i][q] - kernels[i][i] - kernels[q][q]

                if abs(l - r) < 1e-10 or derivative >= 0:
                    continue

                omegas[q] = min(max(omegas[q] - y[q] * (error_i - error_q) / derivative, l), r)

                if abs(omegas[q] - save_omega_q) < eps:
                    continue

                omegas[i] += y[i] * y[q] * (save_omega_q - omegas[q])

                phi_i = y[i] * (omegas[i] - save_omega_i)
                phi_q = y[q] * (omegas[q] - save_omega_q)

                omega_l = omega - error_i - phi_i * kernels[i][i] - phi_q * kernels[i][q]
                omega_r = omega - error_q - phi_i * kernels[i][q] - phi_q * kernels[q][q]

                omega = omega_l if 0 < omegas[i] < c else (omega_r if 0 < omegas[q] < c else (omega_l + omega_r) / 2)
    return omega, omegas
