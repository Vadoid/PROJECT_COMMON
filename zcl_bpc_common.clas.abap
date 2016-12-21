class ZCL_BPC_COMMON definition
  public
  final
  create public .

public section.

  class-methods TEST_AND_DEBUG.
*    changing
*      !CHANGING_DATA type STANDARD TABLE optional .
  class-methods READ_MODEL_DATA
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV optional
      !IT_SEL type UJ0_T_SEL optional
    exporting
      !OUTPUT_DATA type STANDARD TABLE .
  class-methods WRITE_MODEL_DATA
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !TARGET_MODEL type UJ_APPL_ID optional
      !INPUT_DATA type STANDARD TABLE
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !ET_ERROR_RECORDS type STANDARD TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_COMMON IMPLEMENTATION.


METHOD READ_MODEL_DATA.

** 1. **--------- Data Declarations -------**
  DATA: lt_sel TYPE uj0_t_sel, "Selection criteria table
        ls_sel TYPE uj0_s_sel,
        ls_cv TYPE ujk_s_cv,      " Logic Current View
        lt_dim_member TYPE uja_t_dim_member ,
        ls_dim_member LIKE LINE OF lt_dim_member ,
        lo_appl TYPE REF TO cl_uja_application,
        lt_appl_dim TYPE uja_t_appl_dim,
        ls_appl_dim LIKE LINE OF lt_appl_dim,
        lt_dim_name TYPE ujq_t_dim,
        ls_dim_name LIKE LINE OF lt_dim_name,
        lo_model TYPE REF TO if_uj_model,
        lo_dataref TYPE REF TO data,
        lo_query TYPE REF TO if_ujo_query ,
        lv_end_of_data    TYPE rs_bool,
        lt_message TYPE uj0_t_message .

  FIELD-SYMBOLS:  <lt_query_result> TYPE STANDARD TABLE.

**---------------End of Data Declaration----------------------**




*---- 2. Create an object  for  the input parameters such i_appset_id,  i_appl_id.-------*

  CREATE OBJECT lo_appl
    EXPORTING
      i_appset_id      = i_appset_id
      i_application_id = i_appl_id.

*---- 3. Use this object to read the dimension for the  i_appl_id  & Append ' Measures ' to the dimension table -----*

  REFRESH lt_appl_dim.
  lo_appl->get_appl_dim(
  EXPORTING
  i_appl_id   = i_appl_id
  IMPORTING
  et_appl_dim = lt_appl_dim ). "dimension table
  REFRESH lt_dim_name.

**Populate dimension table 'lt_dim_name'.

  LOOP AT lt_appl_dim INTO ls_appl_dim.
    ls_dim_name = ls_appl_dim-dimension.
    APPEND ls_dim_name TO lt_dim_name.
    CLEAR ls_dim_name.
  ENDLOOP.

* Include ' Measures ' as dimension table *
*  ls_dim_name  = 'MEASURES'.
*  APPEND ls_dim_name TO lt_dim_name.
*  SORT lt_dim_name.

*--4. Prepare Selection range table say for ex :  'lt_sel '  *.
* if it_sel[] is initial.
  LOOP AT  lt_dim_name INTO ls_dim_name  .
    CLEAR : ls_cv .

    READ TABLE it_sel INTO ls_sel WITH KEY dimension = ls_dim_name .
    IF sy-subrc = 0.
      LOOP AT it_sel INTO ls_sel WHERE dimension = ls_dim_name .
        APPEND ls_sel TO lt_sel.
      ENDLOOP.
      CONTINUE.
    ENDIF.
* Read from scope for each dimension from current view table*
  IF it_cv is not INITIAL.

    READ TABLE it_cv INTO ls_cv WITH KEY dimension =  ls_dim_name .
    IF sy-subrc = 0 . "and ls_cv-USER_SPECIFIED = abap_true.
      LOOP AT ls_cv-member INTO ls_dim_member.
        ls_sel-dimension = ls_cv-dimension.
        ls_sel-attribute = 'ID'.
        ls_sel-sign = 'I'.
        ls_sel-option = 'EQ'.
        ls_sel-low = ls_dim_member.
        APPEND ls_sel TO lt_sel.
        CLEAR ls_dim_member.
      ENDLOOP.
      CLEAR lt_dim_member.
    ENDIF.
    ENDIF.
  ENDLOOP.


* else.
*   lt_sel[] = it_sel[].
* endif.

*---5. Create a reference structure similar to ct_data using the method -----*

  TRY.
      lo_model = cl_uj_model=>get_model( i_appset_id ).
      lo_model->create_tx_data_ref(
      EXPORTING
      i_appl_name  = i_appl_id
      i_type       = 'T'
      it_dim_name  = lt_dim_name
      if_tech_name = space
      IMPORTING
      er_data      = lo_dataref ).
    CATCH cx_uj_static_check.
  ENDTRY.
* Assigning the structure to table
  ASSIGN lo_dataref->* TO <lt_query_result>.

**Run  a query using method  '  run_rsdri_query ' **
  TRY.

      lo_query = cl_ujo_query_factory=>get_query_adapter(
      i_appset_id = i_appset_id
      i_appl_id   = i_appl_id
      ).
** Run Query to populate ct_data based on dimensions , selection criteria **.

      WHILE lv_end_of_data = rs_c_false.

        lo_query->run_rsdri_query(

        EXPORTING
        it_dim_name       =  lt_dim_name " BPC: Dimension List
        it_range          =  lt_sel     " BPC: Selection condition
        if_check_security = abap_false   " BPC: Generic indicator

        IMPORTING
        et_data           = <lt_query_result>
        e_end_of_data     = lv_end_of_data    " BPC: Last Data Package Yes/No
        et_message        = lt_message    " BPC: Messages
        ).

*        LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
*        APPEND <ls_query_result> TO output_data.
*        ENDLOOP.

     ENDWHILE.
    CATCH cx_ujo_read.  " Exception of common read

  ENDTRY.

*-- 6.  Copy data into output_data ----*

output_data = <lt_query_result>.

ENDMETHOD.


  method TEST_AND_DEBUG.


***************VERSION CONTROL*******************************************
* v.001. Vadim Zaripov - December 2016
*************************************************************************

************** INFORMATION ABOUT CLASS **********************************
* This class consists of various methods which we find useful in BPC
* BADI developments. This class could be called from BADI and parameters
* could be passed to the required methods. This Class id Generic in it's
* nature and could be reused on different implementations with the
* relevant adjustments.
*
* Current list of methods in ZBPC_COMMON_CLASS:
* 1. TEST_AND_DEBUG - service method with basic information and place
*    for testing and debugging. This class is not required and could
*    be delited on customer implementation after the initial unit testing.
*
* 2. READ_MODEL_DATA - reads model data and returns the read values back.
*    Accepts IT_SEL and IT_CV as export parameters to filter data.
*    Environment and Model IDs to be read have to be passed. The data is
*    imported as OUTPUT_DATA Standard Table and has to be assigned to the
*    field symbol or internal table of same type.
*
* 3. WRITE_MODEL_DATA - writes data back to the model. Could be used for
*    cross-model writing. INPUT_DATA is the exporting parameter along with
*    Environment, Model and Target model. If the Target Model is empty,
*    data is written into the same model under exporting parameter.
*    Method should be checking work status and relevant security, returning
*    ET_MESSAGE and ET_ERROR_RECORDS if something went wrong. This will have
*    to be passed to the calling BADI.
*
* 4. READ_MASTER_DATA - read master data of the relevant dimension, pass back.
*    it_sel, Environment, Model and Dimension are the Importing Params, which
*    means that the filter could be passed into method.
*
* 5. READ_MASTER_DATA_CHILDREN - children of the particular hierarchy node,
*    passed in the Importing parameter. Returns all children and can later
*    be reused in READ_MODEL_DATA filter.
*
*
******************* PRE-SET FOR DEBUG ***********************************
DATA:        I_APPSET_ID type UJ_APPSET_ID VALUE 'BAESAI_PLANNING',
             I_APPL_ID type UJ_APPL_ID VALUE 'PROJECTFORECAST'.
*************************************************************************

FIELD-SYMBOLS: <changing_data> TYPE STANDARD TABLE.

************************ASSIGN STRUCTURE OF INCOMING MODEL **************
DATA:  lt_dim_list type uja_t_dim_list,
 lo_appl_mgr type ref to if_uja_application_manager,
 lo_query type ref to if_ujo_query,
 lr_data type ref to data,
 ls_application type UJA_S_APPLICATION,
 ls_dimensions type UJA_S_DIMENSION,
 lt_message TYPE uj0_t_message.

lo_appl_mgr = cl_uja_bpc_admin_factory=>get_application_manager(
 i_appset_id = i_appset_id
 i_application_id = i_appl_id ).
clear ls_application.
lo_appl_mgr->GET(
 exporting
 IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
 IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
 importing
 ES_APPLICATION = ls_application ). " Applications table type

 refresh lt_dim_list.
loop at ls_application-dimensions into ls_dimensions.
 append ls_dimensions-dimension to lt_dim_list.
endloop.
lo_appl_mgr->create_data_ref(
 EXPORTING
 i_data_type = 'T'
 it_dim_name = lt_dim_list
 if_tech_name = abap_false
 if_signeddata = abap_true
 IMPORTING
 er_data = lr_data ).

ASSIGN lr_data->* to <changing_data>.
*****************************************************************************************

*****************************************************************************************
*SAMPLE CALLS
*****************************************************************************************

CALL METHOD ZCL_BPC_COMMON=>READ_MODEL_DATA
      EXPORTING
        I_APPSET_ID = i_appset_id
        I_APPL_ID   = i_appl_id
*        IT_CV       =
*        IT_SEL      =
      IMPORTING
        OUTPUT_DATA = <changing_data>.



CALL METHOD ZCL_BPC_COMMON=>WRITE_MODEL_DATA
  EXPORTING
    I_APPSET_ID      = i_appset_id
    I_APPL_ID        = i_appl_id
"    TARGET_MODEL     =
    INPUT_DATA       = <changing_data>
*  IMPORTING
*    ET_MESSAGE       =
*    ET_ERROR_RECORDS =
    .

************************ CODE SNIPPET TO CONVERT DATA BETWEEN MODELS **********************
***** Based on ENVIRONMENSHELL Dimensionality *********************************************
***** DO NOT USE AS IS ********************************************************************
*   DATA:   lr_member_data TYPE REF TO data,
*           lr_result_rec TYPE REF TO data,
*           lr_input_data type ref to data,
*           lt_final TYPE REF TO data,
*           model TYPE UJ_APPL_ID,
*           call_execute_method type ref to IF_UJ_CUSTOM_LOGIC.
*
*  FIELD-SYMBOLS: <fs_rec> TYPE any,
*                 <input_data> type standard table,
*                 <fs_result_rec> TYPE any,
*                 <commit_data> type standard table,
*                 <ft_final> TYPE STANDARD TABLE.

*field-symbols: <lt_data> type standard table,
* <ls_data> type any,
* <ls_input_data> type any,
* <lv_account> type any,
* <lv_audittrail> type any,
* <lv_category> type any,
* <lv_entity> type any,
* <lv_flow> type any,
* <lv_interco> type any,
* <lv_rptcurrency> type any,
* <lv_scope> type any,
* <lv_time> type any,
* <lv_signeddata> type any.
*
*
*
**ASSIGN input_data to <input_data>.
**create data lr_input_data like line of <input_data>.
**assign lr_input_data->* to <ls_input_data>.
*
**loop at input_data into <ls_input_data>.
**endloop.
*
*
*
*DATA:  lt_dim_list type uja_t_dim_list,
* lo_appl_mgr type ref to if_uja_application_manager,
* lo_query type ref to if_ujo_query,
* lr_data type ref to data,
* ls_application type UJA_S_APPLICATION,
* ls_dimensions type UJA_S_DIMENSION,
* lt_message TYPE uj0_t_message.
*
*lo_appl_mgr = cl_uja_bpc_admin_factory=>get_application_manager(
* i_appset_id = i_appset_id
* i_application_id = i_appl_id ).
*clear ls_application.
*lo_appl_mgr->GET(
* exporting
* IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
* IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
* importing
* ES_APPLICATION = ls_application ). " Applications table type
*
* refresh lt_dim_list.
*loop at ls_application-dimensions into ls_dimensions.
* append ls_dimensions-dimension to lt_dim_list.
*endloop.
*lo_appl_mgr->create_data_ref(
* EXPORTING
* i_data_type = 'T'
* it_dim_name = lt_dim_list
* if_tech_name = abap_false
* if_signeddata = abap_true
* IMPORTING
* er_data = lr_data ).
*
*ASSIGN lr_data->* to <commit_data>.
*
*
*create data lr_data like line of <commit_data>.
*assign lr_data->* to <ls_data>.
** fill each field, by assign a field symbol
*
* ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <ls_data> to <lv_account>.
* ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <ls_data> to <lv_audittrail>.
* ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <ls_data> to <lv_category>.
* ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <ls_data> to <lv_entity>.
* ASSIGN COMPONENT 'FLOW' OF STRUCTURE <ls_data> to <lv_flow>.
* ASSIGN COMPONENT 'INTERCO' OF STRUCTURE <ls_data> to <lv_interco>.
* ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <ls_data> to <lv_rptcurrency>.
* ASSIGN COMPONENT 'SCOPE' OF STRUCTURE <ls_data> to <lv_scope>.
* ASSIGN COMPONENT 'TIME' OF STRUCTURE <ls_data> to <lv_time>.
* ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <ls_data> to <lv_signeddata>.
*
* <lv_account> = 'BS223'.
* <lv_audittrail> = 'Input'.
* <lv_category> = 'Actual'.
* <lv_entity> = 'ZA'.
* <lv_flow> = 'Decrease'.
* <lv_interco> = 'ThirdParty'.
* <lv_rptcurrency> = 'LC'.
* <lv_scope> = 'S_None'.
* <lv_time> = '2006.01'.
* <lv_signeddata> = 500000.
*
*
*append <ls_data> to <commit_data>.


*******************************************************************************************




  endmethod.


METHOD WRITE_MODEL_DATA.


   DATA:   model TYPE UJ_APPL_ID.

  FIELD-SYMBOLS: <commit_data> type standard table.

*Check if Target Model is empty.
*If it is empty this means the sender and target are the same
*No need to have different structure

IF target_model IS INITIAL.

<commit_data> = input_data.
model = i_appl_id.

ELSE.

DATA:  lt_dim_list type uja_t_dim_list,
 lo_appl_mgr type ref to if_uja_application_manager,
 lr_data type ref to data,
 ls_application type UJA_S_APPLICATION,
 ls_dimensions type UJA_S_DIMENSION.

model = target_model.

lo_appl_mgr = cl_uja_bpc_admin_factory=>get_application_manager(
 i_appset_id = i_appset_id
 i_application_id = i_appl_id ).
clear ls_application.
lo_appl_mgr->GET(
 exporting
 IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
 IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
 importing
 ES_APPLICATION = ls_application ). " Applications table type

 refresh lt_dim_list.
loop at ls_application-dimensions into ls_dimensions.
 append ls_dimensions-dimension to lt_dim_list.
endloop.
lo_appl_mgr->create_data_ref(
 EXPORTING
 i_data_type = 'T'
 it_dim_name = lt_dim_list
 if_tech_name = abap_false
 if_signeddata = abap_true
 IMPORTING
 er_data = lr_data ).

ASSIGN lr_data->* to <commit_data>.

ENDIF.





*************************************************
***** WRITE BACK ********************************
DATA: lo_ujo_wb TYPE REF TO if_ujo_write_back,
 ls_wb_param TYPE if_ujo_write_back=>gs_wb_param,
 ls_wb_status TYPE ujo_s_wb_status,
 ls_work_status TYPE ujr_s_work_status,
 ls_audit TYPE ujr_s_update_audit,
 lv_measure TYPE uj_dim_member.

FIELD-SYMBOLS: <lt_error_records> TYPE ANY TABLE.

create data lr_data like <commit_data>.
assign lr_data->* to <lt_error_records>.

ls_work_status-module_id = uj00_c_mod_name_dm.
ls_work_status-blockstatus = 0.
ls_audit-actcode = uju0_cs_act_code-logic_exe.
lo_ujo_wb = cl_ujo_wb_factory=>create_write_back( ).
ls_wb_param = cl_ujo_wb_factory=>default_wb_param( ).
ls_wb_param-work_status = ls_work_status.
ls_wb_param-default_logic = abap_false.
ls_wb_param-update_audit = abap_true.
ls_wb_param-duplicate = abap_true.
ls_wb_param-mdata_check = abap_false.
ls_wb_param-sign_trans = abap_true.
ls_wb_param-measures_formula = lv_measure.
ls_wb_param-audit_info = ls_audit.
ls_wb_param-work_status = ls_work_status.

lo_ujo_wb->write_back(
EXPORTING
 i_appset_id = i_appset_id
 i_appl_id = model
 is_wb_param = ls_wb_param
 it_records = <commit_data>
IMPORTING
 es_wb_status = ls_wb_status
 et_error_records = et_error_records
 et_message = et_message ).



*************************************************
***** WRITE BACK ********************************

ENDMETHOD.
ENDCLASS.