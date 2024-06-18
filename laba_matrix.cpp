#include <iostream>
#include <string>
#include <vector>
#include <random>

void minor(double **matrix, int size, int row, int col, double **newMatrix) {
    int offsetRow = 0; 
    int offsetCol = 0; 
    for(int i = 0; i < size-1; i++) {
        if(i == row) {
            offsetRow = 1; 
        }

        offsetCol = 0;
        for(int j = 0; j < size-1; j++) {
            if(j == col) {
                offsetCol = 1; 
            }

            newMatrix[i][j] = matrix[i + offsetRow][j + offsetCol];
        }
    }
}

double matrixDet(double **matrix, int size) {
    double det = 0;
    double degree = 1; 

    if(size == 1) {
        return matrix[0][0];
    }

    if(size == 2) {
        return matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
    }

    double **newMatrix = new double*[size-1];
    for(int i = 0; i < size-1; i++) {
        newMatrix[i] = new double[size-1];
    }

    for(int j = 0; j < size; j++) {
        minor(matrix, size, 0, j, newMatrix);

        det = det + (degree * matrix[0][j] * matrixDet(newMatrix, size-1));

        degree = -degree;
    }

    for(int i = 0; i < size-1; i++) {
        delete [] newMatrix[i];
    }
    delete [] newMatrix;

    return det;
}

void Complements(double **matrix, double **complements, int size) {
    double **minorMatrix = new double*[size - 1];
    for (int i = 0; i < size - 1; ++i) {
        minorMatrix[i] = new double[size - 1];
    }
    
    for (int i = 0; i < size; ++i) {
        for (int j = 0; j < size; ++j) {
            int minorRow = 0;
            for (int row = 0; row < size; ++row) {
                if (row != i) {
                    int minorCol = 0;
                    for (int col = 0; col < size; ++col) {
                        if (col != j) {
                            minorMatrix[minorRow][minorCol++] = matrix[row][col];
                        }
                    }
                    ++minorRow;
                }
            }
            double sign = ((i + j) % 2 == 0) ? 1 : -1;
            complements[j][i] = sign * matrixDet(minorMatrix, size - 1);
        }
    }
    
}


class Matrix {
    private:
    int rows;
    int cols;
    double **data;
    
    public:
    
    static Matrix Identity(int n, int m) {
        Matrix identity(n, m, 0);
        int min = (n < m) ? n : m;
        for (int i = 0; i < min; ++i) {
            identity.data[i][i] = 1;
        }
        return identity;
    }
    
    static Matrix Zero(int n, int m) {
        return Matrix(n, m, 0);
    }
    
    static Matrix Random(int n, int m) {
        Matrix random(n,m);
        std::default_random_engine gen;
        std::uniform_real_distribution<double> distribution(-5.0, 5.0);
    
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                random.data[i][j] = distribution(gen);
            }
        }
        return random;
    }
    
    static Matrix FromString(std::string str) {
        std::vector<std::vector<double>> tA;
        std::vector<double> row;
        double val = 0;
        int is_digit = 0;
        
        for (char c : str) {
        if (std::isdigit(c) || c == '.') {
            val = val * 10 + (c - '0');
            is_digit = 1;
        }
        else if (c == ',') {
            if (is_digit == 1) {
                row.push_back(val);
                val = 0;
                is_digit = 0;
            }
        }
        else if (c == ']') {
            if (is_digit == 1) {
                row.push_back(val);
                val = 0;
                is_digit = 0;
                tA.push_back(row);
                row.clear();
            }
        }
    }
    
    int rows = tA.size();
    int cols = tA[0].size();

    Matrix A(rows,cols);
    
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            A.data[i][j] = tA[i][j];
        }
    }
    
    return A;
}
    
    Matrix transp() {
        Matrix result(cols, rows);
        for (int i = 0; i < cols; ++i) {
            for (int j = 0; j < rows; ++j) {
                result.data[i][j] = data[j][i];
            }
        }
        return result;
    }
    
    double sum() {
        double sum = 0.0;
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                sum += data[i][j];
            }
        }
        return sum;
    }
    
    Matrix() {
        rows = 0;
        cols = 0;
        data = nullptr;
    }
    
    Matrix(const Matrix& other) {
        rows = other.rows;
        cols = other.cols;
        data = new double*[rows];
        for (int i = 0; i < rows; ++i) {
            data[i] = new double[cols];
            for (int j = 0; j < cols; ++j) {
                data[i][j] = other.data[i][j];
            }
        }
    }
    
    Matrix(Matrix&& other) {
        rows = other.rows;
        cols = other.cols;
        data = other.data;
        
        other.data = nullptr;
        
        
    }
    
    Matrix(int n, int m) {
        rows = n;
        cols = m;
        data = new double*[rows];
        for (int i = 0; i < rows; ++i) {
            data[i] = new double[cols]();
        }
    }
    
    Matrix(int n, int m, double val) {
        rows = n;
        cols = m;
        data = new double*[rows];
        for (int i = 0; i < rows; ++i) {
            data[i] = new double[cols]();
        }
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                data[i][j] = val;
            }
        }
    }
    
    ~Matrix() {
        delete[] data;
    }
    
    double operator()(int i, int j) {
        return data[i][j];
    }
    
    Matrix operator+(const Matrix& other) {
        Matrix result(rows, cols);
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                result.data[i][j] = data[i][j] + other.data[i][j];
            }
        }
        return result;
    }
    
    Matrix operator-(const Matrix& other) {
        Matrix result(rows, cols);
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                result.data[i][j] = data[i][j] - other.data[i][j];
            }
        }
        return result;
    }
    
    Matrix operator*(const Matrix& other) {
        Matrix result(rows, other.cols);
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < other.cols; ++j) {
                for (int k = 0; k < cols; ++k) {
                    result.data[i][j] += data[i][k] * other.data[k][j];
                }
            }
        }
    return result;
}
    
    friend Matrix operator*(double val, const Matrix& matrix) {
        for (int i = 0; i < matrix.rows; ++i) {
            for (int j = 0; j < matrix.cols; ++j) {
                matrix.data[i][j] = val * matrix.data[i][j];
            }
        }
}

    Matrix operator*(double val) {
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                data[i][j] = val * data[i][j];
            }
        }
}

    
    Matrix operator-() {
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                data[i][j] = -data[i][j];
            }
        }
        
    }
    
    bool operator==(const Matrix& other) {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (data[i][j] != other.data[i][j]) {
                    return false;
                }
            }
        }
        return true;
    }
    
    bool operator!=(const Matrix& other) {
        return !(*this == other);
    }
    
    friend std::ostream& operator<<(std::ostream& os, const Matrix& matrix) {
    for (int i = 0; i < matrix.rows; ++i) {
        for (int j = 0; j < matrix.cols; ++j) {
            os << matrix.data[i][j] << " ";
        }
        os << std::endl;
    }
    return os;
}
   
    
};



int main() {
    std::string input;
    std::getline(std::cin, input);
    
    std::vector<std::vector<double>> tA;
    std::vector<double> row;
    double val = 0;
    int is_digit = 0;

    for (char c : input) {
        if (std::isdigit(c) || c == '.') {
            val = val * 10 + (c - '0');
            is_digit = 1;
        }
        else if (c == ',') {
            if (is_digit == 1) {
                row.push_back(val);
                val = 0;
                is_digit = 0;
            }
        }
        else if (c == ']') {
            if (is_digit == 1) {
                row.push_back(val);
                val = 0;
                is_digit = 0;
                tA.push_back(row);
                row.clear();
            }
        }
    }
    
    int rows = tA.size();
    int cols = tA[0].size();

    double **A = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        A[i] = new double[cols];
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            A[i][j] = tA[i][j];
        }
    }
    
    double **B = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        B[i] = new double[cols];
    }
    
    std::default_random_engine gen;
    std::uniform_real_distribution<double> distribution(-5.0, 5.0);
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            B[i][j] = distribution(gen);
        }
    }

    double **AT = new double*[cols];
    for (int i = 0; i < cols; ++i) {
        AT[i] = new double[rows];
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            AT[j][i] = A[i][j];
        }
    }

    double **BT = new double*[cols];
    for (int i = 0; i < cols; ++i) {
        BT[i] = new double[rows];
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            BT[j][i] = B[i][j];
        }
    }

    double **complements = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        complements[i] = new double[cols];
    }

    Complements(AT, complements, rows);

    // for (int i = 0; i < rows; ++i) {
    //     for (int j = 0; j < cols; ++j) {
    //         std::cout << complements[i][j] << " ";
    //     }
    //     std::cout << std::endl;
    // }
    
    double **inverse = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        inverse[i] = new double[cols];
    }
    
    double det_AT = matrixDet(AT, rows);
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            inverse[i][j] = complements[i][j] / det_AT;
        }
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            std::cout << inverse[i][j] << " ";
        }
        std::cout << std::endl;
    }
    
    double **AB = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        AB[i] = new double[cols];
    }
    
    double **BAT = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        BAT[i] = new double[cols];
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            for (int k = 0; k < cols; ++k) {
                AB[i][j] += A[i][k] * B[k][j]; // A * B
            }
        }
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            for (int k = 0; k < cols; ++k) {
                BAT[i][j] += B[i][k] * inverse[k][j]; // B / AT
            }
        }
    }
    
    double sum_a = 0;
    double sum_b = 0;
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            sum_a += A[i][j];
        }
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            BAT[i][j] *= sum_a;
        }
    }
    
    double **F = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        F[i] = new double[cols];
    }
    double **BTA = new double*[rows];
    for (int i = 0; i < rows; ++i) {
        BTA[i] = new double[cols];
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            F[i][j] = AB[i][j] - BAT[i][j];
        }
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            for (int k = 0; k < cols; ++k) {
                BTA[i][j] += BT[i][k] * A[k][j]; // BT * A
            }
        }
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            sum_b += B[i][j];
        }
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            BTA[i][j] /= sum_b;
        }
    }
    
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols;j++) {
            F[i][j] += BTA[i][j];
        }
    }
    
    delete[] A;
    delete[] B;
    delete[] AT;
    delete[] BT;
    delete[] BAT;
    delete[] BTA;
    delete[] F;
    delete[] inverse;
    delete[] complements;
    
    
    Matrix m1(3,3,3);
    Matrix m2(3,3,1);
    
    //std::cout << m1 << std::endl;
    //4 * m1;
    //m1 * 4;
    
    //-m1;
    std::cout << m1 << std::endl;
    //std::cout << m1*m1 << std::endl;
    //std::cout << m1(1,0) << std::endl;
    //std::cout << m1 - m2 << std::endl;
    
    std::cout << Matrix::Random(3,3) << std::endl;
    std::cout << Matrix::FromString("[1, 4, 8], [8, 3, 7], [4, 4, 3]]") << std::endl;   
    std::cout << Matrix::Identity(3,5) << std::endl;
    

}