class ZCL_BPC_COMMON definition
  public
  final
  create public .

public section.

  data I_APPSET_ID type UJ_APPSET_ID .
  data I_APPL_ID type UJ_APPL_ID .
  data I_DIMENSION_ID type UJ_DIM_NAME .

  methods CONSTRUCTOR
    importing
      !I_ENV_ID type UJ_APPSET_ID optional
      !I_MOD_ID type UJ_APPL_ID optional
      !I_DIM_ID type UJ_DIM_NAME optional .
  methods PERIOD_OFFSET
    importing
      !IMPORT_PERIOD type CHAR7
      !OFFSET type UJ_SDATA
    returning
      value(EXPORT_PERIOD) type CHAR7 .
  class-methods TEST_AND_DEBUG .
  methods TARGET_ACCS
    importing
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !TARGET_ACCOUNTS type HASHED TABLE
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods READ_MODEL_DATA
    importing
      !READ_MODEL type UJ_APPL_ID optional
      !IT_CV type UJK_T_CV optional
      !IT_SEL type UJ0_T_SEL optional
    exporting
      value(OUTPUT_DATA) type STANDARD TABLE .
  methods GET_PARENT_MD
    importing
      !PARENT_MBR type UJ_DIM_MEMBER
      !I_DIMENSION_ID type UJ_DIM_NAME optional
    returning
      value(OUTPUT_DATA) type UJA_T_DIM_MEMBER .
  methods TIME
    importing
      !CATEGORY type STRING
      !EXTEND type INT4 optional
    exporting
      !TIMEQRTLY type SORTED TABLE
      !TIMESCOPE type SORTED TABLE
      !FIRST_PERIOD type UJA_S_DIM_MEMBER
      !LAST_PERIOD type UJA_S_DIM_MEMBER
      !CURRENT_PERIOD type UJA_S_DIM_MEMBER .
  methods WRITE_MODEL_DATA
    importing
      !TARGET_MODEL type UJ_APPL_ID optional
      !INPUT_DATA type STANDARD TABLE optional
      !WRITE type CHAR1
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !ET_ERROR_RECORDS type STANDARD TABLE
      !LR_DATA type ref to DATA .
  methods READ_MASTER_DATA
    importing
      !I_DIMENSION_ID type UJ_DIM_NAME
    exporting
      !OUTPUT_R_DATA type ref to DATA
    changing
      !LT_SEL type UJ0_T_SEL optional .
  class-methods READ_MASTER_DATA_HIERARCHY
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_DIMENSION type UJ_DIM_NAME
      !I_PARENT_MBR type UJ_DIM_MEMBER
    exporting
      !OUTPUT_DATA type STANDARD TABLE .
  class-methods CHANGE_LOG_USER
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID .
  class-methods GET_MESSAGE_TEXT
    importing
      !I_MSGNO type SYMSGNO optional
      !I_MSGV1 type SYMSGV optional
      !I_MSGV2 type SYMSGV optional
      !I_MSGV3 type SYMSGV optional
      !I_MSGV4 type SYMSGV optional
      !I_MSGTYPE type SYMSGTY optional
    returning
      value(R_TEXT) type STRING .
  class-methods SWAP_USER
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !I_USER_ID type UJ_LARGE_STRING .
  class-methods READ_ACC_TRANS_RULES
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !TVARVC_NAME type RVARI_VNAM
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !OUTPUT_DATA type HASHED TABLE .
  class-methods READ_ACC_TRANS_RULESV2
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !TVARVC_NAME type RVARI_VNAM
      !EXPAND type CHAR1 optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !OUTPUT_DATA type HASHED TABLE .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_BPC_COMMON IMPLEMENTATION.


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


  method CONSTRUCTOR.

i_appset_id = i_env_id.
i_appl_id = i_mod_id.
i_dimension_id = i_dim_id.


  endmethod.


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


  METHOD GET_PARENT_MD.

     CALL METHOD READ_MASTER_DATA_HIERARCHY
          EXPORTING
            I_APPSET_ID  = I_APPSET_ID
            I_DIMENSION  = I_DIMENSION_ID
            I_PARENT_MBR = PARENT_MBR
          IMPORTING
            OUTPUT_DATA  = OUTPUT_DATA.


  ENDMETHOD.


  method PERIOD_OFFSET.

*IMPORT_PERIOD
*OFFSET
*EXPORT_PERIOD

DATA: SOURCE_MONTH TYPE char2,
      SOURCE_YEAR TYPE char4.

SOURCE_YEAR = IMPORT_PERIOD(4).
SOURCE_MONTH = IMPORT_PERIOD+5(2).


DO OFFSET TIMES.

  SOURCE_MONTH = SOURCE_MONTH + 1.

  IF SOURCE_MONTH = 13.
    SOURCE_YEAR = SOURCE_YEAR + 1.
    SOURCE_MONTH = 01.
  ENDIF.

  ENDDO.

IF STRLEN( SOURCE_MONTH ) = 1.

  CONCATENATE SOURCE_YEAR '.0' SOURCE_MONTH INTO EXPORT_PERIOD.
ELSE.
  CONCATENATE SOURCE_YEAR '.' SOURCE_MONTH INTO EXPORT_PERIOD.
  ENDIF.


  endmethod.


  METHOD READ_ACC_TRANS_RULES.
************************************ READ_ACC_TRANS_RULES START **********************************
******* SIMPLE SAMPLE READ OF ACCOUNT TRANSFORMATION RULES **********************************************

        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID SEQ,
              LT_ACCOUNT_TR_RULES_EXPAND TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID SEQ,
              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
              LS_ACCOUNT_TR_RULES_EXPAND LIKE LINE OF LT_ACCOUNT_TR_RULES_EXPAND,
              L_LOG type string,
              ls_message like line of ET_MESSAGE,
              LS_TVARVC           TYPE TVARVC.
  DATA: ACCOUNTS            TYPE UJA_T_DIM_MEMBER,
        COUNTER TYPE UJ_SMALLINT,
        FLOWS            TYPE UJA_T_DIM_MEMBER,

          WA_MEMBER        TYPE UJ_DIM_MEMBER.



        SELECT SINGLE LOW FROM TVARVC INTO CORRESPONDING FIELDS OF LS_TVARVC WHERE NAME = TVARVC_NAME.
IF sy-subrc NE 0.
          CLEAR: ET_MESSAGE, L_LOG.

        CONCATENATE 'BUSINESS RULES ID FOR CASHFLOW SUBROUTINE 3 NOT FOUND - MAINTAIN TVARVC VARIBALE "'
        TVARVC_NAME '".' INTO L_LOG RESPECTING BLANKS.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

     ls_message-msgid = 'BPC' .
     ls_message-msgty = 'E'.
     ls_message-msgno = '010' .
     ls_message-message = L_LOG.
     APPEND ls_message TO et_message.

       EXIT.
ENDIF.
*********** GET RULES **********************************************
        SELECT APPSET_ID
          APPLICATION_ID
          CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN
          FROM UJP_CALC_ACCOUNT INTO CORRESPONDING FIELDS OF TABLE LT_ACCOUNT_TR_RULES WHERE CALC_ID = LS_TVARVC-LOW.
********************************************************************

CLEAR: LT_ACCOUNT_TR_RULES_EXPAND.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'ACCOUNT'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-ACCOUNT
  IMPORTING
   OUTPUT_DATA  = ACCOUNTS.

IF ACCOUNTS IS NOT INITIAL.

  READ TABLE LT_ACCOUNT_TR_RULES_EXPAND WITH KEY SEQ = LS_ACCOUNT_TR_RULES-SEQ TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
  COUNTER = COUNTER + 1.
  ELSE.
 COUNTER = LS_ACCOUNT_TR_RULES-SEQ.
 ENDIF.

  LOOP AT ACCOUNTS INTO WA_MEMBER.

    IF sy-tabix <> 1.
    COUNTER = COUNTER + 1.
    ENDIF.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-ACCOUNT = WA_MEMBER.
    LS_ACCOUNT_TR_RULES_EXPAND-SEQ = COUNTER.



INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SEQ = LS_ACCOUNT_TR_RULES-SEQ.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.


***********************************************************************************************
CLEAR: LT_ACCOUNT_TR_RULES_EXPAND, LS_ACCOUNT_TR_RULES, COUNTER.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'FLOW'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-SUBTABLES
  IMPORTING
   OUTPUT_DATA  = FLOWS.

IF FLOWS IS NOT INITIAL.

  READ TABLE LT_ACCOUNT_TR_RULES_EXPAND WITH KEY SEQ = LS_ACCOUNT_TR_RULES-SEQ TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
  COUNTER = COUNTER + 1.
  ELSE.
 COUNTER = LS_ACCOUNT_TR_RULES-SEQ.
 ENDIF.

  LOOP AT FLOWS INTO WA_MEMBER.

    IF sy-tabix <> 1.
    COUNTER = COUNTER + 1.
    ENDIF.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-SUBTABLES = WA_MEMBER.
    LS_ACCOUNT_TR_RULES_EXPAND-SEQ = COUNTER.



INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SEQ = LS_ACCOUNT_TR_RULES-SEQ AND SUBTABLES = LS_ACCOUNT_TR_RULES-SUBTABLES.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.

OUTPUT_DATA = LT_ACCOUNT_TR_RULES.


**********************************************************************************************************************
************************************ READ_ACC_TRANS_RULES END   **********************************
  ENDMETHOD.


  METHOD READ_ACC_TRANS_RULESV2.
************************************ READ_ACC_TRANS_RULES START **********************************
******* SIMPLE SAMPLE READ OF ACCOUNT TRANSFORMATION RULES **********************************************

        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
              LT_ACCOUNT_TR_RULES_EXPAND TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
              LS_ACCOUNT_TR_RULES_EXPAND LIKE LINE OF LT_ACCOUNT_TR_RULES_EXPAND,
              L_LOG type string,
              ls_message like line of ET_MESSAGE,
              LS_TVARVC           TYPE TVARVC.
  DATA: ACCOUNTS            TYPE UJA_T_DIM_MEMBER,
        COUNTER TYPE UJ_SMALLINT,
        FLOWS            TYPE UJA_T_DIM_MEMBER,

          WA_MEMBER        TYPE UJ_DIM_MEMBER.



        SELECT SINGLE LOW FROM TVARVC INTO CORRESPONDING FIELDS OF LS_TVARVC WHERE NAME = TVARVC_NAME.
IF sy-subrc NE 0.
          CLEAR: ET_MESSAGE, L_LOG.

        CONCATENATE 'BUSINESS RULES ID FOR CASHFLOW SUBROUTINE 3 NOT FOUND - MAINTAIN TVARVC VARIBALE "'
        TVARVC_NAME '".' INTO L_LOG RESPECTING BLANKS.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

     ls_message-msgid = 'BPC' .
     ls_message-msgty = 'E'.
     ls_message-msgno = '010' .
     ls_message-message = L_LOG.
     APPEND ls_message TO et_message.

       EXIT.
ENDIF.
*********** GET RULES **********************************************
        SELECT APPSET_ID
          APPLICATION_ID
          CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN
          FROM UJP_CALC_ACCOUNT INTO CORRESPONDING FIELDS OF TABLE LT_ACCOUNT_TR_RULES WHERE CALC_ID = LS_TVARVC-LOW.
********************************************************************

CLEAR: LT_ACCOUNT_TR_RULES_EXPAND.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

IF EXPAND = 'Y' OR EXPAND = ''.
  CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'ACCOUNT'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-ACCOUNT
  IMPORTING
   OUTPUT_DATA  = ACCOUNTS.
ENDIF.

IF ACCOUNTS IS NOT INITIAL.

  LOOP AT ACCOUNTS INTO WA_MEMBER.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-ACCOUNT = WA_MEMBER.

INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.


***********************************************************************************************
CLEAR: LT_ACCOUNT_TR_RULES_EXPAND, LS_ACCOUNT_TR_RULES, COUNTER.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

IF EXPAND = 'Y' OR EXPAND = ''.
CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'FLOW'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-SUBTABLES
  IMPORTING
   OUTPUT_DATA  = FLOWS.
ENDIF.

IF FLOWS IS NOT INITIAL.

  LOOP AT FLOWS INTO WA_MEMBER.

     LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-SUBTABLES = WA_MEMBER.


INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SUBTABLES = LS_ACCOUNT_TR_RULES-SUBTABLES.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.

SORT LT_ACCOUNT_TR_RULES BY ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES.
DELETE ADJACENT DUPLICATES FROM LT_ACCOUNT_TR_RULES.

OUTPUT_DATA = LT_ACCOUNT_TR_RULES.


**********************************************************************************************************************
************************************ READ_ACC_TRANS_RULES END   **********************************
  ENDMETHOD.


  METHOD READ_MASTER_DATA.
************************************ READ_MASTER_DATA START****************************************

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
*      lt_sel TYPE uj0_t_sel,
          LS_SEL       TYPE UJ0_S_SEL,
          LR_DATA      TYPE REF TO DATA,
          LS_EMP       TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_DIMESNION_DATA> TYPE STANDARD TABLE.
*               <ls_dimension_data> TYPE ANY.

    REFRESH: LT_ATTR_NAME.

    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = I_DIMENSION_ID.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
*APPEND: 'ID' TO lt_attr_name.

    " Bind the condition data to lt_sel table, this will become selection criteria
    " analogous to the WHERE clause of a DB SELECT statement

* IN THIS EXAMPLE LT_SEL IS CHANGING PARAM, WHICH MIGHT NOT BE THE CASE IN THE FINAL
* IMPLEMENTATION

    IF LT_SEL IS INITIAL.

      LS_SEL-DIMENSION = I_DIMENSION_ID.
      LS_SEL-ATTRIBUTE = 'CALC'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'N'.
      APPEND LS_SEL TO LT_SEL.

    ENDIF.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
*            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
* ASSIGN lr_data->* TO <lt_dimesnion_data>.

    OUTPUT_R_DATA = LR_DATA.

*CREATE DATA LS_EMP LIKE LINE OF <LT_DIMESNION_DATA>.
*ASSIGN LS_EMP->* TO <LS_DIMENSION_DATA>.


  ENDMETHOD.


  METHOD READ_MASTER_DATA_HIERARCHY.

    DATA:    LO_DIM      TYPE REF TO CL_UJA_DIM,
             LR_DATA     TYPE REF TO DATA,
             LR_DIM_DATA TYPE REF TO IF_UJA_DIM_DATA,
             LT_BASE_EN  TYPE UJA_T_DIM_MEMBER.
    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = I_DIMENSION.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = I_PARENT_MBR
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = LT_BASE_EN.

    OUTPUT_DATA = LT_BASE_EN.


************************************ READ_MASTER_DATA END *****************************************

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

"Constructor specifies which model to read - self by defaul
"If READ_MODEL is passed - it means we want to read different model
"Then read different model, which is in READ_MODEL

IF READ_MODEL IS NOT INITIAL.
  I_APPL_ID = READ_MODEL.
ENDIF.

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


  METHOD TARGET_ACCS.

******* EXPAND ACCOUNTS WITH SOURCES
"This method is used to expand the scope of model read with the target accounts for carry-forward
"It recieves the list of accounts, queries business rules and then populates the target accounts
"So that the data could be read from the model with the correct intersection

  DATA: TVARVC_NAME         TYPE RVARI_VNAM.
        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
         LT_ACCOUNT_COMPOUNDED TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,

              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
           WA_MEMBER        TYPE UJ_DIM_MEMBER,
           CV_ACCOUNTS      TYPE UJK_S_CV.



        CONCATENATE I_APPSET_ID '-' I_APPL_ID '-' 'SUB3' INTO TVARVC_NAME.
        CLEAR ET_MESSAGE.


        CALL METHOD READ_ACC_TRANS_RULESV2
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_APPL_ID   = I_APPL_ID
            TVARVC_NAME = TVARVC_NAME
          IMPORTING
            ET_MESSAGE  = ET_MESSAGE
            OUTPUT_DATA = LT_ACCOUNT_TR_RULES.



        IF ET_MESSAGE IS NOT INITIAL.
          RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC .
        ENDIF.

        INSERT LINES OF LT_ACCOUNT_TR_RULES INTO TABLE LT_ACCOUNT_COMPOUNDED.
        CLEAR LT_ACCOUNT_TR_RULES.
*
                CONCATENATE I_APPSET_ID '-' I_APPL_ID '-' 'SUB5' INTO TVARVC_NAME.
        CLEAR ET_MESSAGE.


        CALL METHOD READ_ACC_TRANS_RULESV2
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_APPL_ID   = I_APPL_ID
            EXPAND = 'N'
            TVARVC_NAME = TVARVC_NAME
          IMPORTING
            ET_MESSAGE  = ET_MESSAGE
            OUTPUT_DATA = LT_ACCOUNT_TR_RULES.



        IF ET_MESSAGE IS NOT INITIAL.
          RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC .
        ENDIF.

                INSERT LINES OF LT_ACCOUNT_TR_RULES INTO TABLE LT_ACCOUNT_COMPOUNDED.
        CLEAR LT_ACCOUNT_TR_RULES.


 READ TABLE IT_CV INTO CV_ACCOUNTS WITH TABLE KEY DIM_UPPER_CASE = 'ACCOUNT'.
        LOOP AT CV_ACCOUNTS-MEMBER INTO WA_MEMBER.
           READ TABLE LT_ACCOUNT_COMPOUNDED WITH KEY ACCOUNT = WA_MEMBER TRANSPORTING NO FIELDS.
           IF SY-SUBRC <> 0.
             DELETE LT_ACCOUNT_COMPOUNDED WHERE ACCOUNT = WA_MEMBER.
           ELSE.
            EXIT.
           ENDIF.
        ENDLOOP.

TARGET_ACCOUNTS = LT_ACCOUNT_COMPOUNDED.





  ENDMETHOD.


METHOD TEST_AND_DEBUG.

************************************ TEST_AND_DEBUG START ***************************************

***************VERSION CONTROL*******************************************
* v.001. VZ - December 2016
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
* 5. READ_MASTER_DATA_HIERARCHY - children of the particular hierarchy node,
*    passed in the Importing parameter. Returns all children and can later
*    be reused in READ_MODEL_DATA filter.
*
* 6. READ_ACC_TRANS_RULES - sample read of account transformation rules
*
* 7. SWAP_USER - to swap the user for Data Manager Package
*
* 8. CHANGE_LOG_USER - Change the user back to write the correct data manager log
*
* 9. GET_MESSAGE_TEXT - get relevant error message text from swap user
*
******************** PRE-SET FOR DEBUG ***********************************
*    DATA:        I_APPSET_ID TYPE UJ_APPSET_ID VALUE 'SERCO_PLANNING',
*                 I_APPL_ID   TYPE UJ_APPL_ID VALUE 'CASHFLOW',
*                 LT_SEL      TYPE UJ0_T_SEL,
*                 LD_R_DATA   TYPE REF TO DATA,
*
*                 LS_SEL      TYPE UJ0_S_SEL,
*                 LH_DATA     TYPE UJA_T_DIM_MEMBER,
*                 LS_DATA     TYPE UJA_S_DIM_MEMBER.
*
**************************************************************************
*
*    FIELD-SYMBOLS: <CHANGING_DATA>          TYPE STANDARD TABLE,
*                   <DIMENSION_DATA>         TYPE STANDARD TABLE,
*                   <DIMENSION_HIER_MEMBERS> TYPE STANDARD TABLE.
*
*************************ASSIGN STRUCTURE OF INCOMING MODEL **************
*    DATA:  LT_DIM_LIST    TYPE UJA_T_DIM_LIST,
*           LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
*           LO_QUERY       TYPE REF TO IF_UJO_QUERY,
*           LR_DATA        TYPE REF TO DATA,
*           LS_APPLICATION TYPE UJA_S_APPLICATION,
*           LS_DIMENSIONS  TYPE UJA_S_DIMENSION,
*           LT_MESSAGE     TYPE UJ0_T_MESSAGE.
*
*    LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
*     I_APPSET_ID = I_APPSET_ID
*     I_APPLICATION_ID = I_APPL_ID ).
*    CLEAR LS_APPLICATION.
*    LO_APPL_MGR->GET(
*     EXPORTING
*     IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
*     IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
*     IMPORTING
*     ES_APPLICATION = LS_APPLICATION ). " Applications table type
*
*    REFRESH LT_DIM_LIST.
*    LOOP AT LS_APPLICATION-DIMENSIONS INTO LS_DIMENSIONS.
*      APPEND LS_DIMENSIONS-DIMENSION TO LT_DIM_LIST.
*    ENDLOOP.
*    LO_APPL_MGR->CREATE_DATA_REF(
*     EXPORTING
*     I_DATA_TYPE = 'T'
*     IT_DIM_NAME = LT_DIM_LIST
*     IF_TECH_NAME = ABAP_FALSE
*     IF_SIGNEDDATA = ABAP_TRUE
*     IMPORTING
*     ER_DATA = LR_DATA ).
*
*    ASSIGN LR_DATA->* TO <CHANGING_DATA>.
******************************************************************************************
*
******************************************************************************************
**SAMPLE CALLS
******************************************************************************************
*
*    CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA_HIERARCHY
*      EXPORTING
*        I_APPSET_ID  = I_APPSET_ID
*        I_DIMENSION  = 'EMPLOYEE'
*        I_PARENT_MBR = 'TOTAL_ROLES'
**       IT_CV        =
**       IT_SEL       =
*      IMPORTING
*        OUTPUT_DATA  = LH_DATA.
*
**************************
**SAMPLE LT_SEL FOR DIM
**************************
**APPEND: 'ID' TO lt_attr_name.
*    LOOP AT LH_DATA INTO LS_DATA.
*
*      LS_SEL-DIMENSION = 'EMPLOYEE'.
*      LS_SEL-ATTRIBUTE = 'ID'.
*      LS_SEL-SIGN = 'I'.
*      LS_SEL-OPTION = 'EQ'.
*      LS_SEL-LOW = LS_DATA-DIMENSION.
*      APPEND LS_SEL TO LT_SEL.
*
*    ENDLOOP.
*
********************************
*
*
*
** changing lt_sel might not be required, just for testing purposes now
*
*    CALL METHOD ZCL_BPC_COMMON=>READ_MASTER_DATA
*      EXPORTING
*        I_APPSET_ID   = I_APPSET_ID
*        I_DIMENSION   = 'EMPLOYEE'
**       IT_CV         =
**       IT_SEL        =
*      IMPORTING
*        OUTPUT_R_DATA = LD_R_DATA
*      CHANGING
*        LT_SEL        = LT_SEL.
*
*    ASSIGN LD_R_DATA->* TO <DIMENSION_DATA>.
*



*    CALL METHOD ZCL_BPC_COMMON=>READ_MODEL_DATA
*      EXPORTING
*        I_APPSET_ID = I_APPSET_ID
*        I_APPL_ID   = I_APPL_ID
**       IT_CV       =
**       IT_SEL      =
*      IMPORTING
*        OUTPUT_DATA = <CHANGING_DATA>.
*
*
*
*    CALL METHOD ZCL_BPC_COMMON=>WRITE_MODEL_DATA
*      EXPORTING
*        I_APPSET_ID = I_APPSET_ID
*        I_APPL_ID   = I_APPL_ID
*   "    TARGET_MODEL     =
*        INPUT_DATA  = <CHANGING_DATA>
**  IMPORTING
**       ET_MESSAGE  =
**       ET_ERROR_RECORDS =
*      .

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
****  Idea is to read data from one model and then write it back to another model, converting on the way
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


  METHOD TIME.
"Set time scope depending on the category in IT_CV
***** Time structure definition ********************
    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            CURRMONTH TYPE CHAR32,
            BUDMONTH  TYPE CHAR32,
            TIMEID    TYPE CHAR32,
            VAT_QLY   TYPE CHAR32,
          END OF LS_TIME.
****************************************************

DATA:                LT_TIMESCOPE      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                     LT_TIMEQRTLY      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                     LS_TIMEQRTLY      LIKE LS_TIME,
                     COUNTER           TYPE UJ_SMALLINT,
                     MASTER_DATA       TYPE REF TO DATA.

FIELD-SYMBOLS: <DIMENSION_DATA> TYPE STANDARD TABLE.

***** GET TIME VARIABLES *************************************
    me->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = 'TIME'
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIMESCOPE.

    "Assign TimeScope
    DELETE LT_TIMESCOPE WHERE ID+5(2) = 'IN' OR CURRMONTH = 'A'.
    DELETE LT_TIMESCOPE WHERE CURRMONTH = '' AND BUDMONTH = ''.

    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    CURRENT_PERIOD = LS_TIME-ID. "ALWAYS FIRST PERIOD
******************** END COMMON VARIABLES ********************

    LOOP AT LT_TIMESCOPE INTO LS_TIME.

      IF CATEGORY = 'WFORECAST'.

        IF LS_TIME-BUDMONTH = 'B' AND LS_TIME-CURRMONTH <> 'F'.
          DELETE LT_TIMESCOPE INDEX SY-TABIX.
        ENDIF.

      ELSEIF CATEGORY = 'BUDGET'.

        IF LS_TIME-BUDMONTH <> 'B'.
          DELETE LT_TIMESCOPE INDEX SY-TABIX.
        ENDIF.

      ENDIF.
    ENDLOOP.

    "Assign Time Quarterly
    LT_TIMEQRTLY = LT_TIMESCOPE.
    DELETE LT_TIMEQRTLY WHERE VAT_QLY NE 'Y'.

*GET LAST AND FIRST PERIODS IN TIMEID FORMAT START
    DESCRIBE TABLE LT_TIMESCOPE LINES COUNTER.
    READ TABLE LT_TIMESCOPE INDEX COUNTER INTO LS_TIME.
    LAST_PERIOD = LS_TIME-TIMEID.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    FIRST_PERIOD = LS_TIME-TIMEID.
*GET LAST PERIOD IN TIMEID FORMAT END

TIMESCOPE = LT_TIMESCOPE.
TIMEQRTLY = LT_TIMEQRTLY.



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
             LS_APPLICATION TYPE UJA_S_APPLICATION,
             LS_DIMENSIONS  TYPE UJA_S_DIMENSION.

      MODEL = TARGET_MODEL.

      LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
       I_APPSET_ID = I_APPSET_ID
       I_APPLICATION_ID = MODEL ).
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

IF WRITE = 'Y'.

  <COMMIT_DATA> = INPUT_DATA.

*************************************************
***** WRITE BACK ********************************
    DATA: LO_UJO_WB      TYPE REF TO IF_UJO_WRITE_BACK,
          LS_WB_PARAM    TYPE IF_UJO_WRITE_BACK=>GS_WB_PARAM,
          LS_WB_STATUS   TYPE UJO_S_WB_STATUS,
          LS_WORK_STATUS TYPE UJR_S_WORK_STATUS,
          LS_AUDIT       TYPE UJR_S_UPDATE_AUDIT,
          LV_MEASURE     TYPE UJ_DIM_MEMBER VALUE 'PERIODIC'.

    LS_WORK_STATUS-MODULE_ID = UJ00_C_MOD_NAME_DM.
    LS_WORK_STATUS-BLOCKSTATUS = 0.
    LS_AUDIT-ACTCODE = UJU0_CS_ACT_CODE-LOGIC_EXE.
    LO_UJO_WB = CL_UJO_WB_FACTORY=>CREATE_WRITE_BACK( ).
    LS_WB_PARAM = CL_UJO_WB_FACTORY=>DEFAULT_WB_PARAM( ).
    LS_WB_PARAM-WORK_STATUS = LS_WORK_STATUS.
    LS_WB_PARAM-DEFAULT_LOGIC = ABAP_TRUE.
    LS_WB_PARAM-UPDATE_AUDIT = ABAP_TRUE.
    LS_WB_PARAM-DUPLICATE = ABAP_TRUE.
    LS_WB_PARAM-CALC_DELTA = ABAP_TRUE.
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

ENDIF.

*************************************************
***** WRITE BACK ********************************

************************************ WRITE_MODEL_DATA END ****************************************
  ENDMETHOD.
ENDCLASS.
