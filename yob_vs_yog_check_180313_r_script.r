## library(RODBC)
## library(sqlQuery)
## squibbs <-odbcConnect("squibbs_PRD",uid="ykaslow",pwd="Gungus69")
## rm(test2df, test1df)

yob_yog_no_jive <- sqlQuery(squibbs,
  "WITH base AS 
    (
    SELECT
      e.id_number, e.first_name, e.last_name, 
      SUBSTR(e.birth_dt, 1, 4) AS birth_yr, 
      e.birth_dt, e.record_status_code, 
      e.record_type_code, d.degree_year, d.degree_type,
      d.degree_level_code, SUBSTR(d.grad_dt, 1, 4) as grad_yr, 
      d.grad_dt, e.xcomment, 
      RANK() OVER (PARTITION BY e.id_number ORDER BY d.degree_year ASC, rownum) AS rnk
    FROM 
      advance.entity e
    LEFT JOIN 
      advance.degrees d ON (d.id_number = e.id_number)
    )
    SELECT
      id_number, first_name, last_name, SUBSTR(birth_dt, 1, 4) AS birth_yr, 
      birth_dt, record_status_code, record_type_code, degree_year, degree_type, 
      degree_level_code, SUBSTR(grad_dt, 1, 4) as grad_yr, grad_dt, 
      grad_yr-birth_yr AS grad_age, rnk
    FROM 
      base
    WHERE 
      rnk = '1'
    AND
      birth_dt != '00000000'
    AND
      grad_dt != '00000000'
    AND 
      record_type_code IN ('AL', 'AN')
    ")
