#load "TestRunner.fs"
#load "Ddl.fsx"

Ddl.Ddl.Parser.parse """
CREATE OR REPLACE PACKAGE GMS.GMS_AUDIT IS
/****************************************************************************
*
*	Project	:	PowerGen Gas Supply + Trading
*	Date	:	28/05/97
*	Source	:	gms_audit.pkg
*	Author	:	Martin Morris / Steve Atkins
*	Title	: 	Daily auditing requirements. Definition of procedures
*			for set up of details for a daily audit report.
*
*----------|----------|---------|--------------------------------------------
* Initials | Date     | Version | Comments
*----------|----------|---------|--------------------------------------------
* MM/SA    | 28/05/97 |   1.0   | Initial version
* MM       | 09/05/05 |   1.1   | Amended for Hedge acct. - added paudit008
*
****************************************************************************/
PROCEDURE paudit001 ( p_audit_type     in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit002 ( p_audit_type     in varchar2,
                      p_change_type    in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_price_type     in varchar2,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_val1       in number,
                      p_old_val1       in number,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit003 ( p_audit_type     in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_trad_part_name in varchar2,
                      p_new_val1       in number,
                      p_old_val1       in number,
                      p_new_text1      in varchar2,
                      p_old_text1      in varchar2,
                      p_new_text2      in varchar2,
                      p_old_text2      in varchar2,
                      p_new_text3      in varchar2,
                      p_old_text3      in varchar2,
                      p_new_text4      in varchar2,
                      p_old_text4      in varchar2,
                      p_new_text5      in varchar2,
                      p_old_text5      in varchar2,
                      p_new_text6      in varchar2,
                      p_old_text6      in varchar2,
                      p_new_text7      in varchar2,
                      p_old_text7      in varchar2,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit004 ( p_audit_type     in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_text4      in varchar2,
                      p_new_text6      in varchar2,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit005 ( p_audit_type     in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_val1       in number,
                      p_new_val3       in number,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit006 ( p_audit_type     in varchar2,
                      p_change_type    in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_old_st_date    in date,
                      p_old_end_date   in date,
                      p_new_val1       in number,
                      p_old_val1       in number,
                      p_new_val2       in number,
                      p_old_val2       in number,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit007 ( p_audit_type     in varchar2,
                      p_change_type    in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_price_type     in varchar2,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_val1       in number,
                      p_new_val2       in number,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit008 ( p_audit_type     in varchar2,
                      p_change_type    in varchar2,
                      p_cont_ref       in varchar2,
                      p_meter_point_id in number,
                      p_trad_part_id   in number,
                      p_price_type     in varchar2,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_text1      in varchar2,
                      p_old_text1      in varchar2,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );
PROCEDURE paudit009 ( p_audit_type     in varchar2,
                      p_change_type    in varchar2,
                      p_comm_id        in number,
                      p_trad_part_id   in number,
                      p_price_type     in varchar2,
                      p_new_st_date    in date,
                      p_new_end_date   in date,
                      p_new_text       in varchar2,
                      p_old_text       in varchar2,
                      p_lastup_userid  in varchar2,
                      p_lastup_dt      in date         );

END gms_audit;
/
"""