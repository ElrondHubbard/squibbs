## Find instances of errors in grad, start, stop dates in degrees table
## Proper format = YYYYMMDD ie 20170513 or 20171101
## Error format = (M)MDDYYYY ie 5132017 or 11012017
## Script below finds them, separate script will update to proper format
## Gave both to M Jonson he will check n run

stop_start_grad_date_errors <- sqlQuery(squibbs, "

SELECT
    d.id_number, e.first_name, e.last_name, d.stop_dt, d.start_dt, d.grad_dt
FROM 
    advance.degrees d
LEFT JOIN
    advance.entity e ON (e.id_number = d.id_number)
WHERE
    (LENGTH(d.stop_dt) < 8 OR LENGTH(d.grad_dt) < 8 OR LENGTH(d.start_dt) < 8)

INTERSECT    

SELECT
    d.id_number, e.first_name, e.last_name, d.stop_dt, d.start_dt, d.grad_dt
FROM
    advance.degrees d
LEFT JOIN
    advance.entity e ON (e.id_number = d.id_number)
WHERE
    (SUBSTR(d.grad_dt,1,2) != '20' AND SUBSTR(d.grad_dt,1,2) != '19' AND SUBSTR(d.grad_dt,1,2) != '00')
OR
    (SUBSTR(d.start_dt,1,2) != '20' AND SUBSTR(d.start_dt,1,2) != '19' AND SUBSTR(d.start_dt,1,2) != '00')
OR
    (SUBSTR(d.stop_dt,1,2) != '20' AND SUBSTR(d.stop_dt,1,2) != '19' AND SUBSTR(d.stop_dt,1,2) != '00');

")
