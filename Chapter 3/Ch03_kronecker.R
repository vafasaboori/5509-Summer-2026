I4 <- diag(4)               # 4x4 identity matrix
I4

one4 <- matrix(1, 1, 4)     # matrix(data, nrow, ncol) 1x4 row of ones
one4

zero4 <- matrix(0, 1, 4)    # matrix(data, nrow, ncol) 1x4 row of zeros
zero4

# Tasks (each task assigned to one employee)
employee_matrix <- kronecker(I4, one4)
employee_matrix

# kronecker(A, B) multiplies each element of A by the full matrix B
# More on this: https://youtu.be/G54Ty_yx0Mw?feature=shared

# Employees (each employee does one task)
task_matrix <- kronecker(one4, I4)
task_matrix

# Combine
f.con <- rbind(employee_matrix, task_matrix)
f.con

# Simplified
employee_matrix <- kronecker(diag(4), matrix(1, 1, 4))  # 4 employees × 4 tasks each
task_matrix     <- kronecker(matrix(1, 1, 4), diag(4))  # 4 tasks × 4 employees

f.con <- rbind(employee_matrix, task_matrix)
f.con

