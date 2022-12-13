# My Splitwise

### Table Users

| user_id | user_name | user_surname |
|---------|-----------|--------------|

### Table Users Passwords

| user_id | salt | user_hashed_password |
|---------|------|----------------------|

### Table Groups

| group_id | group_name | group_description |
|----------|------------|-------------------|

### Table Expenses

| expense_id | group_id | expense_name | payer_id | money |
|------------|----------|--------------|----------|-------|

### Table Expenses Partition

| expense_id | user_id | amount |
|------------|---------|--------|

### Table Payments

| group_id | from_user_id | to_user_id | description |
|----------|--------------|------------|-------------|

