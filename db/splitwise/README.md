# Splitwise

### 1. Table Users +

| user_id | user_name | user_email |
|---------|-----------|------------|

### 2. Table Groups +

| group_id | group_name | group_description |
|----------|------------|-------------------|


### 3. Table Invitation to Groups +

| group_id | inviting_person | invited_person |
|----------|-----------------|----------------|

### 4. Table Groups Participants +

| group_id | user_id |
|----------|---------|

### 5. Table Expenses +

| expense_id | group_id | expense_name | payer_id |
|------------|----------|--------------|----------|

### 6. Table Expenses Partition +

| expense_id | user_id | amount |
|------------|---------|--------|

### 7. Table Payments

| group_id | from_user_id | to_user_id | comment |
|----------|--------------|------------|---------|




