## Generate email dupe list 


BB_append <- sqlQuery(squibbs, "
(
  SELECT 
    a2.id_number AS id_number, e2.pref_mail_name AS entity, e2.record_type_code, e2.record_status_code, a2.email_address,
    a2.email_type_code, a2.email_status_code, a2.preferred_ind, SUBSTR(a2.date_added, 1, 10) AS date_added,
    SUBSTR(a2.date_modified, 1, 10) AS date_modified, a2.nbr_bouncebacks, a2.xcomment AS xcomment_email, 
    RANK() OVER (PARTITION BY a2.email_address 
    ORDER BY a2.email_address ASC, a2.id_number ASC, a2.date_added ASC, rownum) AS rnk
  FROM 
    advance.email a2,
    advance.entity e2,
  (
  SELECT x.email email
  FROM( SELECT DISTINCT 
  CASE
    WHEN e.spouse_id_number = ' ' 
    THEN e.id_number
    WHEN e.id_number < e.spouse_id_number 
    THEN e.id_number
    ELSE e.spouse_id_number
    END joint_id, a.email_address email
    FROM advance.entity e, advance.email a
    WHERE e.id_number = a.id_number
    AND a.email_type_code = 'E') x
    GROUP BY x.email
    HAVING COUNT(*) > 1) dup_email_set
    WHERE a2.id_number = e2.id_number
    AND e2.record_status_code <> 'X'
    AND a2.email_address = dup_email_set.email) ")
