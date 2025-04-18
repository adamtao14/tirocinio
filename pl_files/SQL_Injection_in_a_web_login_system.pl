
% Facts: User-provided inputs
user_input('admin').
user_password_input("' OR '1'='1").

% Facts: Database-stored credentials
db_user('admin').
db_password('secret').

% Simulating query construction
construct_query(Username, PasswordInput, Query) :-
    string_concat("SELECT * FROM users WHERE username = '", Username, Q1),
    string_concat(Q1, "' AND password = '", Q2),
    string_concat(Q2, PasswordInput, Q3),
    string_concat(Q3, "';", Query).

% Vulnerable authentication predicate
vulnerable_authenticate(Query) :-
    % Simulating that injection bypasses password check
    sub_string(Query, _, _, _, "' OR '1'='1"),
    sub_string(Query, _, _, _, "SELECT * FROM users WHERE username = 'admin'"),
    !.

% Top-level predicate to check if SQL injection is possible
sql_injection_possible :-
    user_input(Username),
    user_password_input(PasswordInput),
    construct_query(Username, PasswordInput, Query),
    vulnerable_authenticate(Query).
