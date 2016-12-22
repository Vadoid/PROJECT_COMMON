CLASS ZCL_BPC_COMMON DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS TEST_AND_DEBUG.
*    changing
*      !CHANGING_DATA type STANDARD TABLE optional .
    CLASS-METHODS READ_MODEL_DATA
      IMPORTING
        !I_APPSET_ID TYPE UJ_APPSET_ID
        !I_APPL_ID   TYPE UJ_APPL_ID
        !IT_CV       TYPE UJK_T_CV OPTIONAL
        !IT_SEL      TYPE UJ0_T_SEL OPTIONAL
      EXPORTING
        !OUTPUT_DATA TYPE STANDARD TABLE .

    CLASS-METHODS WRITE_MODEL_DATA
      IMPORTING
        !I_APPSET_ID      TYPE UJ_APPSET_ID
        !I_APPL_ID        TYPE UJ_APPL_ID
        !TARGET_MODEL     TYPE UJ_APPL_ID OPTIONAL
        !INPUT_DATA       TYPE STANDARD TABLE
      EXPORTING
        !ET_MESSAGE       TYPE UJ0_T_MESSAGE
        !ET_ERROR_RECORDS TYPE STANDARD TABLE .

    CLASS-METHODS CHANGE_LOG_USER
      IMPORTING
        !I_APPSET_ID TYPE UJ_APPSET_ID
        !I_APPL_ID   TYPE UJ_APPL_ID .
    CLASS-METHODS GET_MESSAGE_TEXT
      IMPORTING
        !I_MSGNO      TYPE SYMSGNO OPTIONAL
        !I_MSGV1      TYPE SYMSGV OPTIONAL
        !I_MSGV2      TYPE SYMSGV OPTIONAL
        !I_MSGV3      TYPE SYMSGV OPTIONAL
        !I_MSGV4      TYPE SYMSGV OPTIONAL
        !I_MSGTYPE    TYPE SYMSGTY OPTIONAL
      RETURNING
        VALUE(R_TEXT) TYPE STRING .
    CLASS-METHODS SWAP_USER
      IMPORTING
        !I_APPSET_ID TYPE UJ_APPSET_ID
        !I_APPL_ID   TYPE UJ_APPL_ID
        !I_USER_ID   TYPE UJ_LARGE_STRING .

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.


CLASS ZCL_BPC_COMMON IMPLEMENTATION.



  METHOD TEST_AND_DEBUG.

************************************ TEST_AND_DEBUG START ***************************************

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
    DATA:        I_APPSET_ID TYPE UJ_APPSET_ID VALUE 'BAESAI_PLANNING',
                 I_APPL_ID   TYPE UJ_APPL_ID VALUE 'PROJECTFORECAST'.
*************************************************************************

    FIELD-SYMBOLS: <CHANGING_DATA> TYPE STANDARD TABLE.

************************ASSIGN STRUCTURE OF INCOMING MODEL **************
    DATA:  LT_DIM_LIST    TYPE UJA_T_DIM_LIST,
           LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
           LO_QUERY       TYPE REF TO IF_UJO_QUERY,
           LR_DATA        TYPE REF TO DATA,
           LS_APPLICATION TYPE UJA_S_APPLICATION,
           LS_DIMENSIONS  TYPE UJA_S_DIMENSION,
           LT_MESSAGE     TYPE UJ0_T_MESSAGE.

    LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
     I_APPSET_ID = I_APPSET_ID
     I_APPLICATION_ID = I_APPL_ID ).
    CLEAR LS_APPLICATION.
    LO_APPL_MGR->GET(
     EXPORTING
     IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
     IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
     IMPORTING
     ES_APPLICATION = LS_APPLICATION ). " Applications table type

    REFRESH LT_DIM_LIST.
    LOOP AT LS_APPLICATION-DIMENSIONS INTO LS_DIMENSIONS.
      APPEND LS_DIMENSIONS-DIMENSION TO LT_DIM_LIST.
    ENDLOOP.
    LO_APPL_MGR->CREATE_DATA_REF(
     EXPORTING
     I_DATA_TYPE = 'T'
     IT_DIM_NAME = LT_DIM_LIST
     IF_TECH_NAME = ABAP_FALSE
     IF_SIGNEDDATA = ABAP_TRUE
     IMPORTING
     ER_DATA = LR_DATA ).

    ASSIGN LR_DATA->* TO <CHANGING_DATA>.
*****************************************************************************************

*****************************************************************************************
*SAMPLE CALLS
*****************************************************************************************

    CALL METHOD ZCL_BPC_COMMON=>READ_MODEL_DATA
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
*       IT_CV       =
*       IT_SEL      =
      IMPORTING
        OUTPUT_DATA = <CHANGING_DATA>.



    CALL METHOD ZCL_BPC_COMMON=>WRITE_MODEL_DATA
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
   "    TARGET_MODEL     =
        INPUT_DATA  = <CHANGING_DATA>
*  IMPORTING
*       ET_MESSAGE  =
*       ET_ERROR_RECORDS =
      .

******************************************************************************************
*REPLACE USER*****************************************************************************
* IT_PARAM IN IF_UJ_CUSTOM_LOGIC~EXECUTE - Replacement Call from standard BADI
* IT_PARAM will give errors if uncommented, unless added as exporting param or
* triggered within IF_UJ_CUSTOM_LOGIC~EXECUTE
******************************************************************************************
*
*  DATA: ls_param        TYPE ujk_s_script_logic_hashentry.
*
*
** Read parameters table to get the user ID of replacement user
*  READ TABLE it_param INTO ls_param WITH KEY hashkey = 'USER'.
*  IF sy-subrc EQ 0 AND ls_param-hashvalue IS NOT INITIAL.
** If user exists - means the first call from script logic, call swap_user method
*    call method ZCL_BPC_COMMON=>swap_user( EXPORTING i_appset_id = i_appset_id
*                             i_appl_id   = i_appl_id
*                             i_user_id   = ls_param-hashvalue ).
*  ELSE.
** If the USER is empty - means the second call to write log for the corect user
*    call method ZCL_BPC_COMMON=>change_log_user( EXPORTING i_appset_id = i_appset_id
*                                 i_appl_id   = i_appl_id ).
*  ENDIF.
********************************************************************************************


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

************************************ TEST_AND_DEBUG END *******************************************
  ENDMETHOD.





  METHOD READ_MODEL_DATA.
************************************ READ_MODEL_DATA START ****************************************

** 1. **--------- Data Declarations -------**
    DATA: LT_SEL         TYPE UJ0_T_SEL, "Selection criteria table
          LS_SEL         TYPE UJ0_S_SEL,
          LS_CV          TYPE UJK_S_CV,      " Logic Current View
          LT_DIM_MEMBER  TYPE UJA_T_DIM_MEMBER,
          LS_DIM_MEMBER  LIKE LINE OF LT_DIM_MEMBER,
          LO_APPL        TYPE REF TO CL_UJA_APPLICATION,
          LT_APPL_DIM    TYPE UJA_T_APPL_DIM,
          LS_APPL_DIM    LIKE LINE OF LT_APPL_DIM,
          LT_DIM_NAME    TYPE UJQ_T_DIM,
          LS_DIM_NAME    LIKE LINE OF LT_DIM_NAME,
          LO_MODEL       TYPE REF TO IF_UJ_MODEL,
          LO_DATAREF     TYPE REF TO DATA,
          LO_QUERY       TYPE REF TO IF_UJO_QUERY,
          LV_END_OF_DATA TYPE RS_BOOL,
          LT_MESSAGE     TYPE UJ0_T_MESSAGE.

    FIELD-SYMBOLS:  <LT_QUERY_RESULT> TYPE STANDARD TABLE.

**---------------End of Data Declaration----------------------**




*---- 2. Create an object  for  the input parameters such i_appset_id,  i_appl_id.-------*

    CREATE OBJECT LO_APPL
      EXPORTING
        I_APPSET_ID      = I_APPSET_ID
        I_APPLICATION_ID = I_APPL_ID.

*---- 3. Use this object to read the dimension for the  i_appl_id  & Append ' Measures ' to the dimension table -----*

    REFRESH LT_APPL_DIM.
    LO_APPL->GET_APPL_DIM(
    EXPORTING
    I_APPL_ID   = I_APPL_ID
    IMPORTING
    ET_APPL_DIM = LT_APPL_DIM ). "dimension table
    REFRESH LT_DIM_NAME.

**Populate dimension table 'lt_dim_name'.

    LOOP AT LT_APPL_DIM INTO LS_APPL_DIM.
      LS_DIM_NAME = LS_APPL_DIM-DIMENSION.
      APPEND LS_DIM_NAME TO LT_DIM_NAME.
      CLEAR LS_DIM_NAME.
    ENDLOOP.

* Include ' Measures ' as dimension table *
*  ls_dim_name  = 'MEASURES'.
*  APPEND ls_dim_name TO lt_dim_name.
*  SORT lt_dim_name.

*--4. Prepare Selection range table say for ex :  'lt_sel '  *.
* if it_sel[] is initial.
    LOOP AT  LT_DIM_NAME INTO LS_DIM_NAME  .
      CLEAR : LS_CV .

      READ TABLE IT_SEL INTO LS_SEL WITH KEY DIMENSION = LS_DIM_NAME .
      IF SY-SUBRC = 0.
        LOOP AT IT_SEL INTO LS_SEL WHERE DIMENSION = LS_DIM_NAME .
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.
        CONTINUE.
      ENDIF.
* Read from scope for each dimension from current view table*
      IF IT_CV IS NOT INITIAL.

        READ TABLE IT_CV INTO LS_CV WITH KEY DIMENSION =  LS_DIM_NAME .
        IF SY-SUBRC = 0 . "and ls_cv-USER_SPECIFIED = abap_true.
          LOOP AT LS_CV-MEMBER INTO LS_DIM_MEMBER.
            LS_SEL-DIMENSION = LS_CV-DIMENSION.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = LS_DIM_MEMBER.
            APPEND LS_SEL TO LT_SEL.
            CLEAR LS_DIM_MEMBER.
          ENDLOOP.
          CLEAR LT_DIM_MEMBER.
        ENDIF.
      ENDIF.
    ENDLOOP.


* else.
*   lt_sel[] = it_sel[].
* endif.

*---5. Create a reference structure similar to ct_data using the method -----*

    TRY.
        LO_MODEL = CL_UJ_MODEL=>GET_MODEL( I_APPSET_ID ).
        LO_MODEL->CREATE_TX_DATA_REF(
        EXPORTING
        I_APPL_NAME  = I_APPL_ID
        I_TYPE       = 'T'
        IT_DIM_NAME  = LT_DIM_NAME
        IF_TECH_NAME = SPACE
        IMPORTING
        ER_DATA      = LO_DATAREF ).
      CATCH CX_UJ_STATIC_CHECK.
    ENDTRY.
* Assigning the structure to table
    ASSIGN LO_DATAREF->* TO <LT_QUERY_RESULT>.

**Run  a query using method  '  run_rsdri_query ' **
    TRY.

        LO_QUERY = CL_UJO_QUERY_FACTORY=>GET_QUERY_ADAPTER(
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
        ).
** Run Query to populate ct_data based on dimensions , selection criteria **.

        WHILE LV_END_OF_DATA = RS_C_FALSE.

          LO_QUERY->RUN_RSDRI_QUERY(

          EXPORTING
          IT_DIM_NAME       =  LT_DIM_NAME " BPC: Dimension List
          IT_RANGE          =  LT_SEL     " BPC: Selection condition
          IF_CHECK_SECURITY = ABAP_FALSE   " BPC: Generic indicator

          IMPORTING
          ET_DATA           = <LT_QUERY_RESULT>
          E_END_OF_DATA     = LV_END_OF_DATA    " BPC: Last Data Package Yes/No
          ET_MESSAGE        = LT_MESSAGE    " BPC: Messages
          ).

*        LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
*        APPEND <ls_query_result> TO output_data.
*        ENDLOOP.

        ENDWHILE.
      CATCH CX_UJO_READ.  " Exception of common read

    ENDTRY.

*-- 6.  Copy data into output_data ----*

    OUTPUT_DATA = <LT_QUERY_RESULT>.

************************************ READ_MODEL_DATA END ******************************************
  ENDMETHOD.


  METHOD WRITE_MODEL_DATA.
************************************ WRITE_MODEL_DATA START ***************************************

    DATA:   MODEL TYPE UJ_APPL_ID.

    FIELD-SYMBOLS: <COMMIT_DATA> TYPE STANDARD TABLE.

*Check if Target Model is empty.
*If it is empty this means the sender and target are the same
*No need to have different structure

    IF TARGET_MODEL IS INITIAL.

      ASSIGN INPUT_DATA TO <COMMIT_DATA>.
      MODEL = I_APPL_ID.

    ELSE.

      DATA:  LT_DIM_LIST    TYPE UJA_T_DIM_LIST,
             LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
             LR_DATA        TYPE REF TO DATA,
             LS_APPLICATION TYPE UJA_S_APPLICATION,
             LS_DIMENSIONS  TYPE UJA_S_DIMENSION.

      MODEL = TARGET_MODEL.

      LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
       I_APPSET_ID = I_APPSET_ID
       I_APPLICATION_ID = I_APPL_ID ).
      CLEAR LS_APPLICATION.
      LO_APPL_MGR->GET(
       EXPORTING
       IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
       IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
       IMPORTING
       ES_APPLICATION = LS_APPLICATION ). " Applications table type

      REFRESH LT_DIM_LIST.
      LOOP AT LS_APPLICATION-DIMENSIONS INTO LS_DIMENSIONS.
        APPEND LS_DIMENSIONS-DIMENSION TO LT_DIM_LIST.
      ENDLOOP.
      LO_APPL_MGR->CREATE_DATA_REF(
       EXPORTING
       I_DATA_TYPE = 'T'
       IT_DIM_NAME = LT_DIM_LIST
       IF_TECH_NAME = ABAP_FALSE
       IF_SIGNEDDATA = ABAP_TRUE
       IMPORTING
       ER_DATA = LR_DATA ).

      ASSIGN LR_DATA->* TO <COMMIT_DATA>.

    ENDIF.


*************************************************
***** WRITE BACK ********************************
    DATA: LO_UJO_WB      TYPE REF TO IF_UJO_WRITE_BACK,
          LS_WB_PARAM    TYPE IF_UJO_WRITE_BACK=>GS_WB_PARAM,
          LS_WB_STATUS   TYPE UJO_S_WB_STATUS,
          LS_WORK_STATUS TYPE UJR_S_WORK_STATUS,
          LS_AUDIT       TYPE UJR_S_UPDATE_AUDIT,
          LV_MEASURE     TYPE UJ_DIM_MEMBER.

    LS_WORK_STATUS-MODULE_ID = UJ00_C_MOD_NAME_DM.
    LS_WORK_STATUS-BLOCKSTATUS = 0.
    LS_AUDIT-ACTCODE = UJU0_CS_ACT_CODE-LOGIC_EXE.
    LO_UJO_WB = CL_UJO_WB_FACTORY=>CREATE_WRITE_BACK( ).
    LS_WB_PARAM = CL_UJO_WB_FACTORY=>DEFAULT_WB_PARAM( ).
    LS_WB_PARAM-WORK_STATUS = LS_WORK_STATUS.
    LS_WB_PARAM-DEFAULT_LOGIC = ABAP_FALSE.
    LS_WB_PARAM-UPDATE_AUDIT = ABAP_TRUE.
    LS_WB_PARAM-DUPLICATE = ABAP_TRUE.
    LS_WB_PARAM-MDATA_CHECK = ABAP_FALSE.
    LS_WB_PARAM-SIGN_TRANS = ABAP_TRUE.
    LS_WB_PARAM-MEASURES_FORMULA = LV_MEASURE.
    LS_WB_PARAM-AUDIT_INFO = LS_AUDIT.
    LS_WB_PARAM-WORK_STATUS = LS_WORK_STATUS.

    LO_UJO_WB->WRITE_BACK(
    EXPORTING
     I_APPSET_ID = I_APPSET_ID
     I_APPL_ID = MODEL
     IS_WB_PARAM = LS_WB_PARAM
     IT_RECORDS = <COMMIT_DATA>
    IMPORTING
     ES_WB_STATUS = LS_WB_STATUS
     ET_ERROR_RECORDS = ET_ERROR_RECORDS
     ET_MESSAGE = ET_MESSAGE ).



*************************************************
***** WRITE BACK ********************************

************************************ WRITE_MODEL_DATA END ****************************************
  ENDMETHOD.


  METHOD CHANGE_LOG_USER.
************************************ CHANGE_LOG_USER START ***************************************

    DATA: CONTEXT_RO TYPE REF TO IF_UJ_CONTEXT,
          L_LOG      TYPE STRING,
          L_PATH     TYPE STRING,
          L_STRING1  TYPE STRING,
          L_STRING2  TYPE STRING,
          L_STRING3  TYPE STRING,
          LENGTH     TYPE I,
          L_PATH2    TYPE UJ_DOCNAME,
          RESULT_TAB TYPE MATCH_RESULT_TAB,
          LS_RESULT  TYPE MATCH_RESULT,
          L_USER     TYPE UJ0_S_USER.


* Get the current context details
    CALL METHOD CL_UJ_CONTEXT=>GET_CUR_CONTEXT
      RECEIVING
        RO_CONTEXT = CONTEXT_RO.
    CONTEXT_RO->SWITCH_TO_SRVADMIN( ).

* Assign the user details
    L_USER = CONTEXT_RO->DS_USER.
    IF L_USER-USER_ID NE SY-UNAME.
      L_USER-USER_ID = SY-UNAME.


      TRY.
          CALL METHOD CL_UJ_CONTEXT=>SET_CUR_CONTEXT
            EXPORTING
              I_APPSET_ID = I_APPSET_ID
              IS_USER     = L_USER
              I_APPL_ID   = I_APPL_ID.

        CATCH CX_UJ_OBJ_NOT_FOUND .
          L_LOG = GET_MESSAGE_TEXT( I_MSGTYPE = 'E'  I_MSGNO = SY-MSGNO ).
          CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      ENDTRY.

      L_PATH = CL_UJK_MODEL=>G_LOG_PATH.
      FIND SY-UNAME IN L_PATH.
      IF SY-SUBRC NE 0.

        SPLIT L_PATH AT '\PRIVATEPUBLICATIONS\' INTO L_STRING1 L_STRING2.
        FIND FIRST OCCURRENCE OF '\' IN L_STRING2  RESULTS RESULT_TAB.
        READ TABLE RESULT_TAB INTO LS_RESULT INDEX 1.
        LENGTH = STRLEN( L_STRING2 ) - LS_RESULT-OFFSET.
        L_STRING3 = L_STRING2+LS_RESULT-OFFSET(LENGTH).
        CONCATENATE L_STRING1 '\PRIVATEPUBLICATIONS\' SY-UNAME L_STRING3 INTO L_PATH2.
        CL_UJK_MODEL=>G_LOG_PATH = L_PATH2.
        TRY .
            CL_UJK_LOGGER=>SAVE_LOG( I_PATH = L_PATH2 ).
          CATCH CX_UJ_STATIC_CHECK.

        ENDTRY.


      ENDIF.
    ENDIF.

************************************ CHANGE_LOG_USER END ******************************************
  ENDMETHOD.

  METHOD GET_MESSAGE_TEXT.
************************************ GET_MESSAGE_TEXT START ***************************************

    DATA:
      L_MSGTYPE       TYPE SY-MSGTY VALUE 'I',
      L_MSGV1         TYPE SY-MSGV1,
      L_MSGV2         TYPE SY-MSGV2,
      L_MSGV3         TYPE SY-MSGV3,
      L_MSGV4         TYPE SY-MSGV4,
      P_MESSAGE_CLASS TYPE SY-MSGID VALUE 'UJD_EXCEPTION'.


    IF I_MSGTYPE IS SUPPLIED.
      L_MSGTYPE = I_MSGTYPE.
    ENDIF.

    IF I_MSGV1 IS SUPPLIED AND I_MSGV2 IS SUPPLIED AND I_MSGV3 IS SUPPLIED AND I_MSGV4 IS SUPPLIED.
      L_MSGV1 = I_MSGV1. L_MSGV2 = I_MSGV2. L_MSGV3 = I_MSGV3. L_MSGV4 = I_MSGV4.
      MESSAGE ID P_MESSAGE_CLASS TYPE L_MSGTYPE NUMBER I_MSGNO
            INTO R_TEXT
            WITH L_MSGV1 L_MSGV2 L_MSGV3 L_MSGV4.
    ELSEIF I_MSGV1 IS SUPPLIED AND I_MSGV2 IS SUPPLIED AND I_MSGV3 IS SUPPLIED AND I_MSGV4 IS NOT SUPPLIED.
      L_MSGV1 = I_MSGV1. L_MSGV2 = I_MSGV2. L_MSGV3 = I_MSGV3.
      MESSAGE ID P_MESSAGE_CLASS TYPE L_MSGTYPE NUMBER I_MSGNO
            INTO R_TEXT
            WITH L_MSGV1 L_MSGV2 L_MSGV3.
    ELSEIF I_MSGV1 IS SUPPLIED AND I_MSGV2 IS SUPPLIED AND I_MSGV3 IS NOT SUPPLIED AND I_MSGV4 IS NOT SUPPLIED.
      L_MSGV1 = I_MSGV1. L_MSGV2 = I_MSGV2.
      MESSAGE ID P_MESSAGE_CLASS TYPE L_MSGTYPE NUMBER I_MSGNO
            INTO R_TEXT
            WITH L_MSGV1 L_MSGV2.
    ELSEIF I_MSGV1 IS SUPPLIED AND I_MSGV2 IS NOT SUPPLIED AND I_MSGV3 IS NOT SUPPLIED AND I_MSGV4 IS NOT SUPPLIED.
      L_MSGV1 = I_MSGV1.
      MESSAGE ID P_MESSAGE_CLASS TYPE L_MSGTYPE NUMBER I_MSGNO
            INTO R_TEXT
            WITH L_MSGV1.
    ELSEIF I_MSGV1 IS NOT SUPPLIED AND I_MSGV2 IS NOT SUPPLIED AND I_MSGV3 IS NOT SUPPLIED AND I_MSGV4 IS NOT SUPPLIED.
      MESSAGE ID P_MESSAGE_CLASS TYPE L_MSGTYPE NUMBER I_MSGNO
            INTO R_TEXT.
    ELSE.
      "error error
    ENDIF.

************************************ GET_MESSAGE_TEXT END ***************************************
  ENDMETHOD.

  METHOD SWAP_USER.
************************************ SWAP_USER START ********************************************

    DATA: CONTEXT_RO TYPE REF TO IF_UJ_CONTEXT,
          L_LOG      TYPE STRING,
          LS_PARAM   TYPE UJK_S_SCRIPT_LOGIC_HASHENTRY,
          L_USER_ID  TYPE STRING,
          L_USER     TYPE UJ0_S_USER.

    L_USER_ID = I_USER_ID.


* Get the current context details
    CALL METHOD CL_UJ_CONTEXT=>GET_CUR_CONTEXT
      RECEIVING
        RO_CONTEXT = CONTEXT_RO.
    CONTEXT_RO->SWITCH_TO_SRVADMIN( ).

* Assign the user details
    L_USER = CONTEXT_RO->DS_USER.
    L_USER-USER_ID = L_USER_ID.

    TRY.
        CALL METHOD CL_UJ_CONTEXT=>SET_CUR_CONTEXT
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            IS_USER     = L_USER
            I_APPL_ID   = I_APPL_ID.

      CATCH CX_UJ_OBJ_NOT_FOUND .
        L_LOG = GET_MESSAGE_TEXT( I_MSGTYPE = 'E'  I_MSGNO = SY-MSGNO ).
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
    ENDTRY.

************************************ SWAP_USER END ***************************************

  ENDMETHOD.



ENDCLASS.