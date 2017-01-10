class ZCL_BPC_COMPLEX_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJ_CUSTOM_LOGIC .

  class-methods CLEAR_DATA
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_CURRTRANS
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_CURRTRANS_RD
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_FCSTSNAPSHOT
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_FXTRANS
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_FXTRANS3
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods C_WARSTATUSCHANGE
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  class-methods GET_INFLATION
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
    exporting
      !ET_INFLATION type ZBPC_PF_T .
  class-methods GET_MD
    importing
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE
    exporting
      !TMPROJECTS type STANDARD TABLE
      !TIMEACT type STANDARD TABLE
      !TIMECURR type STRING
      !TIMEPRIOR type STRING
      !TIMEFCST type STANDARD TABLE
      !TIMECURRINP type STRING
      !T_CURR type STANDARD TABLE
      !R_CURR type STANDARD TABLE
      !ALL_R_CURR type STANDARD TABLE
      !CALC_BASE type STRING
      !TRANSAUDIT type STANDARD TABLE .
  class-methods GET_MODEL_DATA
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !LT_SEL type UJ0_T_SEL
    exporting
      !ET_RATES type ZBPC_RATES_T
      !ET_PF type ZBPC_PF_T
      !ET_RD type ZBPC_RD_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_COMPLEX_BADI IMPLEMENTATION.


  method CLEAR_DATA.

  DATA:  LR_REC        TYPE REF TO DATA,
           LR_RESULT_REC TYPE REF TO DATA,
           LT_FINAL      TYPE REF TO DATA,
           lent_value TYPE UJ_SDATA,
           lcurr_value TYPE UJ_SDATA.

    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.

    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.

    ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <ls_result_rec> to <ls_signeddata>.
   <ls_signeddata> = 0.

   APPEND <ls_result_rec> TO <lt_final>.
    ENDLOOP.

    ct_data = <lt_final>.



  endmethod.


  METHOD C_CURRTRANS.

    DATA: BEGIN OF LS_PROJECT,
            ID       TYPE CHAR32,
            CURRENCY TYPE CHAR32,
          END OF LS_PROJECT.

    DATA: BEGIN OF LS_COST_ELEMENT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_COST_ELEMENT.

    DATA: BEGIN OF LS_CURRENCY,
            ID        TYPE CHAR32,
            TC_LOOKUP TYPE CHAR32,
          END OF LS_CURRENCY.

    DATA: LT_PROJECT      LIKE TABLE OF LS_PROJECT,
          LT_COST_ELEMENT LIKE TABLE OF LS_COST_ELEMENT,
          LT_CURRENCY     LIKE TABLE OF LS_CURRENCY.


    DATA: LS_SEL              TYPE UJ0_S_SEL,
          LT_SEL              TYPE UJ0_T_SEL,
          ZS_BPC_RATES        TYPE ZBPC_RATES,
          ZT_BPC_RATES        TYPE ZBPC_RATES_T,
          ZS_BPC_PF           TYPE ZBPC_PF,
          ZT_BPC_PF           TYPE ZBPC_PF_T,
          ZS_MARKUP           TYPE ZBPC_PF,
          ZT_MARKUP           TYPE ZBPC_PF_T,
          ZT_BPC_PF_INFLATION TYPE ZBPC_PF_T.

    DATA:  TMPROJECTS  TYPE STANDARD TABLE OF CHAR32,
           TIMEACT     TYPE STANDARD TABLE OF CHAR32,
           TIMECURR    TYPE STRING,
           TIMEFCST    TYPE STANDARD TABLE OF CHAR32,
           TIMEPRIOR   TYPE STRING,
           TIMECURRINP TYPE STRING,
           T_CURR      TYPE STANDARD TABLE OF CHAR32,
           R_CURR      TYPE STANDARD TABLE OF CHAR32,
           ALL_R_CURR  TYPE STANDARD TABLE OF CHAR32,
           CALC_BASE   TYPE STRING,
           TRANSAUDIT  TYPE STANDARD TABLE OF CHAR32.

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


    DATA: COST_EXPENSES TYPE UJA_T_DIM_MEMBER.

*****************GET COST EXPENSES******************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'EXPENSES'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_EXPENSES.
****************************************************

*****************GET FULL PROJECT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_PROJECT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'PROJECT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'CURRENCY' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'PROJECT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_PROJECT.
      APPEND LS_PROJECT TO LT_PROJECT.
    ENDLOOP.

****************************************************

*****************GET FULL COST ELEMENT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_COST_ELEMENT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RATETYPE' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'COST_ELEMENT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_COST_ELEMENT.
      APPEND LS_COST_ELEMENT TO LT_COST_ELEMENT.
    ENDLOOP.

****************************************************

*****************GET FULL CURRENCY MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_CURRENCY.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'CURRENCY'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'TC_LOOKUP' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'CURRENCY'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_CURRENCY.
      APPEND LS_CURRENCY TO LT_CURRENCY.
    ENDLOOP.

****************************************************

*****************GET MASTER DATA SELECTIONS*********
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MD
          EXPORTING
            IT_PARAM    = IT_PARAM
          IMPORTING
            TMPROJECTS  = TMPROJECTS
            TIMECURR    = TIMECURR
            TIMEACT     = TIMEACT
            TIMEFCST    = TIMEFCST
            TIMEPRIOR   = TIMEPRIOR
            TIMECURRINP = TIMECURRINP
            T_CURR      = T_CURR
            R_CURR      = R_CURR
            ALL_R_CURR  = ALL_R_CURR
            CALC_BASE   = CALC_BASE
            TRANSAUDIT  = TRANSAUDIT.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
****************************************************

***************** GET INFLATION ********************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_INFLATION
          EXPORTING
            I_APPL_ID    = 'PROJECTFORECAST'
            I_APPSET_ID  = I_APPSET_ID
          IMPORTING
            ET_INFLATION = ZT_BPC_PF_INFLATION.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

****************** GET RATES *************************
    CLEAR: LS_SEL, LT_SEL.

*TAKE ONLY WFORECAST
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-LOW = 'WFORECAST'.
    APPEND LS_SEL TO LT_SEL.
*TAKE ONLY WFORECAST

    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'Rates'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_RATES    = ZT_BPC_RATES.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************


****************** GET TM_MARKUP *************************
    CLEAR: LS_SEL, LT_SEL.

    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.

    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-LOW = 'TM_MARKUP'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-LOW = 'XXXX.INP'.
    APPEND LS_SEL TO LT_SEL.

    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'PROJECTFORECAST'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_PF       = ZT_MARKUP.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************


******************************************************
******************************************************
******************************************************
    DATA:  LR_REC          TYPE REF TO DATA,
           LR_RESULT_REC   TYPE REF TO DATA,
           LT_FINAL        TYPE REF TO DATA,
           LENT_VALUE      TYPE UJ_SDATA,
           LCURR_VALUE     TYPE UJ_SDATA,
           RATE_VALUE      TYPE UJ_SDATA,
           SIGNEDDATA_KEPT TYPE UJ_SDATA,
           CURRENCY_KEPT   TYPE CHAR32,
           AUDITTRAIL_KEPT TYPE CHAR32.

    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <LS_TIME>         TYPE ANY,
                   <LS_CATEGORY>     TYPE ANY,
                   <LS_PROJECT>      TYPE ANY,
                   <LS_LINE_ITEM>    TYPE ANY,
                   <LS_CURRENCY>     TYPE ANY,
                   <LS_AUDITTRAIL>   TYPE ANY,
                   <LS_COST_ELEMENT> TYPE ANY.



    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.

    CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_CURRENCY, ZS_BPC_RATES.
* Loop thro incoming data and create result set
    LOOP AT CT_DATA ASSIGNING  <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.

      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'LINE_ITEM' OF STRUCTURE <LS_RESULT_REC> TO <LS_LINE_ITEM>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CURRENCY>.
      ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <LS_RESULT_REC> TO <LS_COST_ELEMENT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.


***********************************************************************************
***********************************************************************************
      CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_CURRENCY, ZS_BPC_RATES, ZS_MARKUP, RATE_VALUE, LENT_VALUE, LCURR_VALUE.

      " CHECK <ls_signeddata> <> 0.


      READ TABLE TMPROJECTS WITH KEY TABLE_LINE = <LS_PROJECT> INTO LS_PROJECT.
      IF LS_PROJECT IS NOT INITIAL.
        READ TABLE LT_PROJECT WITH KEY ID = LS_PROJECT-ID INTO LS_PROJECT.
      ENDIF.

      READ TABLE COST_EXPENSES WITH KEY TABLE_LINE = <LS_COST_ELEMENT> INTO LS_COST_ELEMENT.
      IF LS_COST_ELEMENT IS NOT INITIAL.
        READ TABLE LT_COST_ELEMENT WITH KEY ID = LS_COST_ELEMENT-ID INTO LS_COST_ELEMENT.
      ENDIF.

      READ TABLE T_CURR WITH KEY TABLE_LINE = <LS_CURRENCY> INTO LS_CURRENCY.
      IF LS_CURRENCY IS NOT INITIAL.
        READ TABLE LT_CURRENCY WITH KEY ID = LS_CURRENCY-ID INTO LS_CURRENCY.
      ENDIF.

      SIGNEDDATA_KEPT = <LS_SIGNEDDATA>.
      CURRENCY_KEPT = <LS_CURRENCY>.
      AUDITTRAIL_KEPT = <LS_AUDITTRAIL>.
***********************************************************************************
***********************************************************************************

      IF LS_PROJECT IS NOT INITIAL.

        IF LS_COST_ELEMENT IS NOT INITIAL.

***********************************************************
***********************************************************
          IF LS_CURRENCY IS NOT INITIAL.

            READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_PROJECT-CURRENCY R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
            LENT_VALUE = ZS_BPC_RATES-SIGNEDDATA.

            CLEAR ZS_BPC_RATES.

            READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_CURRENCY-TC_LOOKUP R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
            LCURR_VALUE = ZS_BPC_RATES-SIGNEDDATA.

            IF LENT_VALUE = 0 OR LCURR_VALUE = 0.
              <LS_SIGNEDDATA> = 0.
              RATE_VALUE = 0.
            ELSE.
              RATE_VALUE = ( LENT_VALUE / LCURR_VALUE ).
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * RATE_VALUE.
            ENDIF.
            <LS_CURRENCY> = 'LC'.
            <LS_AUDITTRAIL> = 'TRANS_INPUT'.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "******************************TM_MARKUP***************************"
            " 1. First
            READ TABLE ZT_MARKUP WITH KEY PROJECT = <LS_PROJECT> COST_ELEMENT = <LS_COST_ELEMENT> LINE_ITEM = <LS_LINE_ITEM> CURRENCY = CURRENCY_KEPT INTO ZS_MARKUP.

            <LS_SIGNEDDATA> = SIGNEDDATA_KEPT * ( 1 + ZS_MARKUP-SIGNEDDATA ).
            <LS_CURRENCY> = CURRENCY_KEPT.
            <LS_AUDITTRAIL> = 'INPUT_CHG_EXP_TCURR'.
            <LS_COST_ELEMENT> = 'CE_601000'.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            " 2. Second

            <LS_SIGNEDDATA> = SIGNEDDATA_KEPT * ( 1 + ZS_MARKUP-SIGNEDDATA ) * RATE_VALUE.
            <LS_CURRENCY> = 'LC'.
            <LS_AUDITTRAIL> = 'INPUT_CHG_EXP_TCURR'.
            <LS_COST_ELEMENT> = 'CE_601000'.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "******************************************************************"

          ELSEIF <LS_CURRENCY> = 'LC'.

            <LS_CURRENCY> = 'LC'.
            <LS_AUDITTRAIL> = 'TRANS_INPUT'.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            READ TABLE ZT_MARKUP WITH KEY PROJECT = <LS_PROJECT> COST_ELEMENT = <LS_COST_ELEMENT>  LINE_ITEM = <LS_LINE_ITEM> CURRENCY = <LS_CURRENCY> INTO ZS_MARKUP.

            <LS_SIGNEDDATA> =  <LS_SIGNEDDATA> * ( 1 + ZS_MARKUP-SIGNEDDATA ).
            <LS_AUDITTRAIL> = 'INPUT_CHG_EXP_LC'.
            <LS_COST_ELEMENT> = 'CE_601000'.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


          ENDIF.
***********************************************************
***********************************************************

        ELSE. "COST_ELEMENT

          READ TABLE LT_COST_ELEMENT WITH KEY ID = <LS_COST_ELEMENT> INTO LS_COST_ELEMENT.
***********************************************************
***********************************************************
          IF LS_CURRENCY IS NOT INITIAL.

            READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_PROJECT-CURRENCY R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
            LENT_VALUE = ZS_BPC_RATES-SIGNEDDATA.

            CLEAR ZS_BPC_RATES.

            READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_CURRENCY-TC_LOOKUP R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
            LCURR_VALUE = ZS_BPC_RATES-SIGNEDDATA.

            IF LENT_VALUE = 0 OR LCURR_VALUE = 0.
              <LS_SIGNEDDATA> = 0.
              RATE_VALUE = 0.
            ELSE.
              RATE_VALUE = ( LENT_VALUE / LCURR_VALUE ).
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * RATE_VALUE.
            ENDIF.
            <LS_CURRENCY> = 'LC'.
            <LS_AUDITTRAIL> = 'TRANS_INPUT'.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ELSEIF <LS_CURRENCY> = 'LC'.

            <LS_CURRENCY> = 'LC'.
            <LS_AUDITTRAIL> = 'TRANS_INPUT'.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
***********************************************************
***********************************************************


        ENDIF.


      ELSE. "PROJECTS

        READ TABLE LT_PROJECT WITH KEY ID = <LS_PROJECT> INTO LS_PROJECT.
***********************************************************
***********************************************************
        IF LS_CURRENCY IS NOT INITIAL.

          READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_PROJECT-CURRENCY R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
          LENT_VALUE = ZS_BPC_RATES-SIGNEDDATA.

          CLEAR ZS_BPC_RATES.

          READ TABLE ZT_BPC_RATES WITH KEY INPUTCURRENCY = LS_CURRENCY-TC_LOOKUP R_ACCOUNT = LS_COST_ELEMENT-RATETYPE TIME = <LS_TIME> INTO ZS_BPC_RATES.
          LCURR_VALUE = ZS_BPC_RATES-SIGNEDDATA.

          IF LENT_VALUE = 0 OR LCURR_VALUE = 0.
            <LS_SIGNEDDATA> = 0.
          ELSE.
            <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * ( LENT_VALUE / LCURR_VALUE ).
          ENDIF.
          <LS_CURRENCY> = 'LC'.
          <LS_AUDITTRAIL> = 'TRANS_INPUT'.

          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ELSEIF <LS_CURRENCY> = 'LC'.

          <LS_CURRENCY> = 'LC'.
          <LS_AUDITTRAIL> = 'TRANS_INPUT'.

          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ENDIF.
***********************************************************
***********************************************************



      ENDIF. "PROJECTS

    ENDLOOP.
* Send the result data back. BPC always over-writes existing value. So, send
* the latest values.
    "  clear: zt_bpc_pf, zs_bps_pf.
    " loop at <lt_final> into zs
    "APPEND LINES OF <lt_final> to ct_data.

    CT_DATA = <LT_FINAL>.

    CLEAR: <LT_FINAL>.

    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.

      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CURRENCY>.
      ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <LS_RESULT_REC> TO <LS_COST_ELEMENT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      "CHECK <ls_signeddata> <> 0.
      CHECK <LS_COST_ELEMENT> = 'CE_601000'.
      CHECK <LS_CURRENCY> = 'LC'.
      CHECK <LS_AUDITTRAIL> = 'INPUT_CHG_EXP_LC' OR <LS_AUDITTRAIL> = 'INPUT_CHG_EXP_TCURR'.
      CHECK <LS_CATEGORY> = 'WFORECAST'.

      <LS_AUDITTRAIL> = 'INPUT_CHG_EXP'.

      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


    ENDLOOP.

    APPEND LINES OF <LT_FINAL> TO CT_DATA.

  ENDMETHOD.


  METHOD C_CURRTRANS_RD.

    DATA: BEGIN OF LS_PROJECT,
            ID         TYPE CHAR32,
            L1CURRENCY TYPE CHAR32,
            L1_ID TYPE CHAR32,
            CURRENCY   TYPE CHAR32,
          END OF LS_PROJECT.

    DATA: BEGIN OF LS_RESOURCE_ACCT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_RESOURCE_ACCT.

     DATA: BEGIN OF LS_CURRENCY,
            ID        TYPE CHAR32,
            TC_LOOKUP TYPE CHAR32,
          END OF LS_CURRENCY.



    DATA: LT_PROJECT      LIKE TABLE OF LS_PROJECT,
          ls_RESOURCE_ACCT2 LIKE ls_RESOURCE_ACCT,
          LT_RESOURCE_ACCT LIKE TABLE OF LS_RESOURCE_ACCT,
          LT_CURRENCY     LIKE TABLE OF LS_CURRENCY.


    DATA: LS_SEL              TYPE UJ0_S_SEL,
          LT_SEL              TYPE UJ0_T_SEL,
          ZS_BPC_RATES        TYPE ZBPC_RATES,
          ZT_BPC_RATES        TYPE ZBPC_RATES_T,
          ZS_BPC_PF           TYPE ZBPC_PF,
          ZT_BPC_PF           TYPE ZBPC_PF_T,
          ZT_BPC_PF_INFLATION TYPE ZBPC_PF_T.

    DATA:  TMPROJECTS  TYPE STANDARD TABLE OF CHAR32,
           TIMEACT     TYPE STANDARD TABLE OF CHAR32,
           TIMECURR    TYPE STRING,
           TIMEFCST    TYPE STANDARD TABLE OF CHAR32,
           TIMEPRIOR   TYPE STRING,
           TIMECURRINP TYPE STRING,
           T_CURR      TYPE STANDARD TABLE OF CHAR32,
           R_CURR      TYPE STANDARD TABLE OF CHAR32,
           ALL_R_CURR  TYPE STANDARD TABLE OF CHAR32,
           CALC_BASE   TYPE STRING,
           TRANSAUDIT  TYPE STANDARD TABLE OF CHAR32.

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


    DATA: RESOURCE_ACCT_TMSALES               TYPE UJA_T_DIM_MEMBER,
          RESOURCE_ACCT_LABOURCOST                  TYPE UJA_T_DIM_MEMBER.




*****************GET MASTER DATA SELECTIONS*********
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MD
          EXPORTING
            IT_PARAM    = IT_PARAM
          IMPORTING
            TMPROJECTS  = TMPROJECTS
            TIMECURR    = TIMECURR
            TIMEACT     = TIMEACT
            TIMEFCST    = TIMEFCST
            TIMEPRIOR   = TIMEPRIOR
            TIMECURRINP = TIMECURRINP
            T_CURR      = T_CURR
            R_CURR      = R_CURR
            ALL_R_CURR  = ALL_R_CURR
            CALC_BASE   = CALC_BASE
            TRANSAUDIT  = TRANSAUDIT.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
****************************************************


*****************GET TMSALES / LABOURCOST ******************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'RESOURCE_ACCT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'TOTAL_LABOURCOST'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = RESOURCE_ACCT_LABOURCOST.

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'RESOURCE_ACCT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'TOTAL_TMSALES'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = RESOURCE_ACCT_TMSALES.


****************************************************


*****************GET FULL PROJECT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_PROJECT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'PROJECT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'CURRENCY' TO LT_ATTR_NAME,
            'L1_ID' TO LT_ATTR_NAME,
            'L1CURRENCY' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'PROJECT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_PROJECT.
      APPEND LS_PROJECT TO LT_PROJECT.
    ENDLOOP.

****************************************************

*****************GET FULL COST ELEMENT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_RESOURCE_ACCT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'RESOURCE_ACCT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RATETYPE' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_RESOURCE_ACCT.
      APPEND LS_RESOURCE_ACCT TO LT_RESOURCE_ACCT.
    ENDLOOP.

****************************************************

*****************GET FULL CURRENCY MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_CURRENCY.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'CURRENCY'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'TC_LOOKUP' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'CURRENCY'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_CURRENCY.
      APPEND LS_CURRENCY TO LT_CURRENCY.
    ENDLOOP.

****************************************************


***************** GET INFLATION ********************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_INFLATION
          EXPORTING
            I_APPL_ID    = 'PROJECTFORECAST'
            I_APPSET_ID  = I_APPSET_ID
          IMPORTING
            ET_INFLATION = ZT_BPC_PF_INFLATION.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

****************** GET RATES *************************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'Rates'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_RATES    = ZT_BPC_RATES.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

******************************************************
******************************************************
******************************************************
    DATA:  LR_REC          TYPE REF TO DATA,
           LR_RESULT_REC   TYPE REF TO DATA,
           LT_FINAL        TYPE REF TO DATA,
           LINE_ITEM       TYPE CHAR32,
           FXRATETGT_VALUE      TYPE UJ_SDATA,
           FXRATESRC_VALUE      TYPE UJ_SDATA,
           FXRATEL1_VALUE      TYPE UJ_SDATA,
           inflation_value     TYPE UJ_SDATA,
           inflation_time      TYPE string,
           SIGNEDDATA_KEPT TYPE UJ_SDATA,
           CURRENCY_KEPT   TYPE CHAR32,
           ZTIME TYPE CHAR32,
           AUDITTRAIL_KEPT TYPE CHAR32.

    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <LS_TIME_WEEK>         TYPE ANY,
                   <LS_CATEGORY>     TYPE ANY,
                   <LS_PROJECT>      TYPE ANY,
                   <LS_CURRENCY>     TYPE ANY,
                   <LS_RESOURCE_ACCT> TYPE ANY.




    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.

    CLEAR: LS_PROJECT, LS_RESOURCE_ACCT, LS_CURRENCY, ZS_BPC_RATES.
* Loop thro incoming data and create result set
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'TIME_WEEK' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME_WEEK>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CURRENCY>.
      ASSIGN COMPONENT 'RESOURCE_ACCT' OF STRUCTURE <LS_RESULT_REC> TO <LS_RESOURCE_ACCT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.


***********************************************************************************
***********************************************************************************
      CLEAR: LS_PROJECT, LS_RESOURCE_ACCT, LS_RESOURCE_ACCT2, LS_CURRENCY, ZS_BPC_PF, ZS_BPC_RATES, FXRATESRC_VALUE, FXRATETGT_VALUE, FXRATEL1_VALUE, LINE_ITEM, inflation_value.

      "Change regarding INP period not translating
       IF <ls_time_week>+5(3) = 'INP'.
          ZTIME = <ls_time_week>.
       ELSE.
           ZTIME = <ls_time_week>(7).
       ENDIF.
       "End Change

      READ TABLE LT_PROJECT WITH KEY ID = <LS_PROJECT> INTO LS_PROJECT.


        read table RESOURCE_ACCT_TMSALES with key table_line = <ls_RESOURCE_ACCT> into ls_RESOURCE_ACCT2.
          if ls_RESOURCE_ACCT2 is NOT initial.
             read table lt_RESOURCE_ACCT with key id = ls_RESOURCE_ACCT2-id into ls_RESOURCE_ACCT2.
          ENDIF.

         IF LS_RESOURCE_ACCT2 IS INITIAL.

         read table RESOURCE_ACCT_LABOURCOST with key table_line = <ls_RESOURCE_ACCT> into ls_RESOURCE_ACCT.


         if ls_RESOURCE_ACCT is NOT initial.
             read table lt_RESOURCE_ACCT with key id = ls_RESOURCE_ACCT-id into ls_RESOURCE_ACCT.
         ENDIF.

         READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_RESOURCE_ACCT-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
          fxratesrc_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_RESOURCE_ACCT-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
          fxratel1_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

         ELSE.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_RESOURCE_ACCT2-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
          fxratesrc_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_RESOURCE_ACCT2-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
          fxratel1_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

         ENDIF.

      CONCATENATE <ls_time_week>(5) 'INP' INTO inflation_time.
      SIGNEDDATA_KEPT = <LS_SIGNEDDATA>.
      CURRENCY_KEPT = <LS_CURRENCY>.

***********************************************************************************
***********************************************************************************

     IF <ls_RESOURCE_ACCT> = ls_RESOURCE_ACCT2-id.

         LOOP AT r_curr INTO ls_currency-id.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_RESOURCE_ACCT2-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.


           <ls_signeddata> = signeddata_kept * ( fxratetgt_value /  fxratesrc_value ).
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


           read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

             <ls_signeddata> = signeddata_kept * ( fxratel1_value / fxratesrc_value ).
             <ls_currency> = ls_project-l1currency.


             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.


      ELSEIF <ls_RESOURCE_ACCT> = ls_RESOURCE_ACCT-id.


           READ TABLE ZT_BPC_PF_INFLATION WITH key project = ls_project-l1_id time = inflation_time INTO ZS_BPC_PF.
           inflation_value = ZS_BPC_PF-signeddata.
           clear ZS_BPC_PF.

           "************************
            if inflation_value = 0.
              inflation_value = 1.
            endif.
           "*************************


          LOOP AT r_curr INTO ls_currency-id.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_RESOURCE_ACCT-ratetype category = 'WFORECAST' r_entity = 'Global' time = ZTIME INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.

           <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratetgt_value /  fxratesrc_value ) ).
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


          read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

            <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratel1_value / fxratesrc_value ) ).
             <ls_currency> = ls_project-l1currency.


             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.


       ENDIF.

    ENDLOOP.
* Send the result data back. BPC always over-writes existing value. So, send
* the latest values.
    APPEND LINES OF <LT_FINAL> TO CT_DATA.
    "CT_DATA = <LT_FINAL>.


  ENDMETHOD.


  METHOD C_FCSTSNAPSHOT.

    DATA: BEGIN OF LS_PROJECT,
            ID         TYPE CHAR32,
            L1CURRENCY TYPE CHAR32,
            L1_ID      TYPE CHAR32,
            WBS_STATUS TYPE CHAR32,
            CURRENCY   TYPE CHAR32,

          END OF LS_PROJECT.

    DATA: BEGIN OF LS_TIME,
            ID           TYPE CHAR32,
            PERIOD       TYPE CHAR32,
            COPYFORECAST TYPE CHAR32,
          END OF LS_TIME.

    DATA: LT_PROJECT      LIKE SORTED TABLE OF LS_PROJECT WITH UNIQUE KEY ID,
          LT_PROJECT_TEMP LIKE SORTED TABLE OF LS_PROJECT WITH UNIQUE KEY ID,
          LS_PROJECT_L1   LIKE LS_PROJECT,
          LT_TIME         LIKE TABLE OF LS_TIME,
          FLAG            TYPE CHAR1.


    DATA: LS_SEL   TYPE UJ0_S_SEL,
          LT_SEL   TYPE UJ0_T_SEL,
          ZS_FLAGS TYPE ZBPC_PF,
          ZT_FLAGS TYPE ZBPC_PF_T.



    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    DATA:   COST_FLAGS                TYPE UJA_T_DIM_MEMBER.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


*****************GET FLAG COST_ELEMENTS******************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'TOTAL_FLAGS'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_FLAGS.

*****************GET FULL PROJECT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_PROJECT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'PROJECT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
  APPEND: 'ID' TO LT_ATTR_NAME,
  'L1CURRENCY' TO LT_ATTR_NAME,
  'L1_ID' TO LT_ATTR_NAME,
  'WBS_STATUS' TO LT_ATTR_NAME,
  'CURRENCY' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'PROJECT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.


    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.

    CLEAR LT_PROJECT_TEMP.

   MOVE-CORRESPONDING <LT_MD> TO LT_PROJECT.
    LOOP AT LT_PROJECT INTO LS_PROJECT.
      If ls_project-l1_id = ls_project-id.
      CHECK LS_PROJECT-WBS_STATUS <> 'CLOSED'.
      endif.
      APPEND LS_PROJECT TO LT_PROJECT_TEMP.
    ENDLOOP.

    LT_PROJECT = LT_PROJECT_TEMP.
*  LOOP AT <LT_MD> ASSIGNING <LS_MD>.
*    MOVE-CORRESPONDING <LS_MD> TO LS_PROJECT.
*    APPEND LS_PROJECT TO LT_PROJECT.
*  ENDLOOP.

****************************************************

*****************GET TIME MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_TIME.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'TIME'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'COPYFORECAST' TO LT_ATTR_NAME,
            'PERIOD' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'CURRMONTH'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'C'.
    APPEND LS_SEL TO LT_SEL.


    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_TIME.
      APPEND LS_TIME TO LT_TIME.
    ENDLOOP.

****************************************************

****************** GET FLAGS *************************
    CLEAR: LS_SEL, LT_SEL.

    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.

    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-LOW = 'APPROVE'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-LOW = 'WFORECAST'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'COST_ELEMENT'.
    LS_SEL-LOW = 'CE_1023'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CURRENCY'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-LOW = LS_TIME-ID.
    APPEND LS_SEL TO LT_SEL.

    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'PROJECTFORECAST'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_PF       = ZT_FLAGS.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************
******************************************************
******************************************************
******************************************************
    DATA:  LR_REC        TYPE REF TO DATA,
           LR_RESULT_REC TYPE REF TO DATA,
           LT_FINAL      TYPE REF TO DATA.


    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <LS_TIME>         TYPE ANY,
                   <LS_CATEGORY>     TYPE ANY,
                   <LS_PROJECT>      TYPE ANY,
                   <LS_CURRENCY>     TYPE ANY,
                   <LS_AUDITTRAIL>   TYPE ANY,
                   <LS_COST_ELEMENT> TYPE ANY,
                   <LS_LINE_ITEM>    TYPE ANY.



    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.


    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <ls_rec>.

      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <LS_RESULT_REC> TO <LS_COST_ELEMENT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      CLEAR: LS_PROJECT, ZS_FLAGS.

      CHECK <LS_SIGNEDDATA> <> 0.

      READ TABLE LT_PROJECT WITH TABLE KEY ID = <LS_PROJECT> INTO LS_PROJECT.

      READ TABLE LT_PROJECT WITH TABLE KEY ID = LS_PROJECT-L1_ID TRANSPORTING NO FIELDS.
      CHECK SY-SUBRC = 0.

      READ TABLE ZT_FLAGS WITH KEY PROJECT = LS_PROJECT-L1_ID INTO ZS_FLAGS.
      CHECK ZS_FLAGS IS NOT INITIAL.

      IF ZS_FLAGS-SIGNEDDATA = 1 OR ZS_FLAGS-SIGNEDDATA = 2. "OR ZS_FLAGS-SIGNEDDATA = 3.

        READ TABLE COST_FLAGS WITH KEY TABLE_LINE = <LS_COST_ELEMENT> TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          <LS_AUDITTRAIL> = 'FCST_FLAG'.
          <LS_CATEGORY> = LS_TIME-COPYFORECAST.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
          <LS_AUDITTRAIL> = 'INPUT'.
          <LS_CATEGORY> = LS_TIME-COPYFORECAST.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CT_DATA = <LT_FINAL>.


  ENDMETHOD.


  METHOD C_FXTRANS.

***********************************************************************************
*Original version - May 2016 VZ.
*1. CNANGE BPC-3 - FXTRANS to take ACTUAL rates to convert ACTUALS. 20-07-2016. VZ.
***********************************************************************************



    DATA: BEGIN OF LS_PROJECT,
            ID         TYPE CHAR32,
            L1CURRENCY TYPE CHAR32,
            L1_ID TYPE CHAR32,
            CURRENCY   TYPE CHAR32,
          END OF LS_PROJECT.

    DATA: BEGIN OF LS_COST_ELEMENT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_COST_ELEMENT.

     DATA: BEGIN OF LS_CURRENCY,
            ID        TYPE CHAR32,
            TC_LOOKUP TYPE CHAR32,
          END OF LS_CURRENCY.

    DATA: BEGIN OF LS_AUDITTRAIL,
            ID     TYPE CHAR32,
            RPT_ID TYPE CHAR32,
          END OF LS_AUDITTRAIL.

    DATA: LT_PROJECT      LIKE TABLE OF LS_PROJECT,
          LT_COST_ELEMENT LIKE TABLE OF LS_COST_ELEMENT,
          LT_CURRENCY     LIKE TABLE OF LS_CURRENCY,
          LT_AUDITTRAIL   LIKE TABLE OF LS_AUDITTRAIL.


    DATA: LS_SEL              TYPE UJ0_S_SEL,
          LT_SEL              TYPE UJ0_T_SEL,
          ZS_BPC_RATES        TYPE ZBPC_RATES,
          ZT_BPC_RATES        TYPE ZBPC_RATES_T,
          ZS_BPC_PF           TYPE ZBPC_PF,
          ZT_BPC_PF           TYPE ZBPC_PF_T,
          ZS_MARKUP           TYPE ZBPC_PF,
          ZT_MARKUP           TYPE ZBPC_PF_T,
          ZT_BPC_PF_INFLATION TYPE ZBPC_PF_T.

    DATA:  TMPROJECTS  TYPE STANDARD TABLE OF CHAR32,
           TIMEACT     TYPE STANDARD TABLE OF CHAR32,
           TIMECURR    TYPE STRING,
           TIMEFCST    TYPE STANDARD TABLE OF CHAR32,
           TIMEPRIOR   TYPE STRING,
           TIMECURRINP TYPE STRING,
           T_CURR      TYPE STANDARD TABLE OF CHAR32,
           R_CURR      TYPE STANDARD TABLE OF CHAR32,
           ALL_R_CURR  TYPE STANDARD TABLE OF CHAR32,
           CALC_BASE   TYPE STRING,
           TRANSAUDIT  TYPE STANDARD TABLE OF CHAR32.

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


*****************GET MASTER DATA SELECTIONS*********
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MD
          EXPORTING
            IT_PARAM    = IT_PARAM
          IMPORTING
            TMPROJECTS  = TMPROJECTS
            TIMECURR    = TIMECURR
            TIMEACT     = TIMEACT
            TIMEFCST    = TIMEFCST
            TIMEPRIOR   = TIMEPRIOR
            TIMECURRINP = TIMECURRINP
            T_CURR      = T_CURR
            R_CURR      = R_CURR
            ALL_R_CURR  = ALL_R_CURR
            CALC_BASE   = CALC_BASE
            TRANSAUDIT  = TRANSAUDIT.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
****************************************************

*****************GET FULL PROJECT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_PROJECT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'PROJECT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'CURRENCY' TO LT_ATTR_NAME,
            'L1_ID' TO LT_ATTR_NAME,
            'L1CURRENCY' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'PROJECT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_PROJECT.
      APPEND LS_PROJECT TO LT_PROJECT.
    ENDLOOP.

****************************************************

*****************GET FULL COST ELEMENT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_COST_ELEMENT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RATETYPE' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'COST_ELEMENT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_COST_ELEMENT.
      APPEND LS_COST_ELEMENT TO LT_COST_ELEMENT.
    ENDLOOP.

****************************************************

*****************GET FULL CURRENCY MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_CURRENCY.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'CURRENCY'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'TC_LOOKUP' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'CURRENCY'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_CURRENCY.
      APPEND LS_CURRENCY TO LT_CURRENCY.
    ENDLOOP.

****************************************************

*****************GET AUDITTRAIL MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_AUDITTRAIL.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'AUDITTRAIL'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RPT_ID' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_AUDITTRAIL.
      APPEND LS_AUDITTRAIL TO LT_AUDITTRAIL.
    ENDLOOP.

****************************************************


****************** GET RATES *************************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'Rates'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_RATES    = ZT_BPC_RATES.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

******************************************************
******************************************************
******************************************************
    DATA:  LR_REC          TYPE REF TO DATA,
           LR_RESULT_REC   TYPE REF TO DATA,
           LT_FINAL        TYPE REF TO DATA,
           LINE_ITEM       TYPE CHAR32,
           FXRATETGT_VALUE      TYPE UJ_SDATA,
           FXRATESRC_VALUE      TYPE UJ_SDATA,
           FXRATEL1_VALUE      TYPE UJ_SDATA,
           inflation_value     TYPE UJ_SDATA,
           inflation_time      TYPE string,
           SIGNEDDATA_KEPT TYPE UJ_SDATA,
           CURRENCY_KEPT   TYPE CHAR32,
           AUDITTRAIL_KEPT TYPE CHAR32.

    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <LS_TIME>         TYPE ANY,
                   <LS_CATEGORY>     TYPE ANY,
                   <LS_PROJECT>      TYPE ANY,
                   <LS_CURRENCY>     TYPE ANY,
                   <LS_AUDITTRAIL>   TYPE ANY,
                   <LS_COST_ELEMENT> TYPE ANY,
                   <LS_LINE_ITEM>    TYPE ANY.



    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.

    CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_CURRENCY, ZS_BPC_RATES.
* Loop thro incoming data and create result set
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'LINE_ITEM' OF STRUCTURE <LS_RESULT_REC> TO <LS_LINE_ITEM>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CURRENCY>.
      ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <LS_RESULT_REC> TO <LS_COST_ELEMENT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.


***********************************************************************************
***********************************************************************************
      CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_CURRENCY, LS_AUDITTRAIL, ZS_BPC_PF, ZS_BPC_RATES, FXRATESRC_VALUE, FXRATETGT_VALUE, FXRATEL1_VALUE, LINE_ITEM.

      CHECK <ls_project> <> 'NO_PROJECT'.

      READ TABLE LT_PROJECT WITH KEY ID = <LS_PROJECT> INTO LS_PROJECT.
      READ TABLE LT_AUDITTRAIL WITH KEY ID = <LS_AUDITTRAIL> INTO LS_AUDITTRAIL.
      READ TABLE LT_COST_ELEMENT WITH KEY ID = <LS_COST_ELEMENT> INTO LS_COST_ELEMENT.

*********************************************************************************
*CNANGE BPC-3 - FXTRANS to take ACTUAL rates to convert ACTUALS. 20-07-2016. VZ.
*********************************************************************************

*           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
*          fxratesrc_value = zs_bpc_rates-signeddata.
*
*           clear zs_bpc_rates.
*
*          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
*          fxratel1_value = zs_bpc_rates-signeddata.
*
*           clear zs_bpc_rates.

*BPC-3 Insert Start*********************************************************************************
           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_cost_element-ratetype category = <LS_CATEGORY> r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratesrc_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_cost_element-ratetype category = <LS_CATEGORY> r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratel1_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.
*BPC-3 Insert End*********************************************************************************

      SIGNEDDATA_KEPT = <LS_SIGNEDDATA>.
      CURRENCY_KEPT = <LS_CURRENCY>.
      AUDITTRAIL_KEPT = <LS_AUDITTRAIL>.
***********************************************************************************
***********************************************************************************


         LOOP AT r_curr INTO ls_currency-id.


*      READ TABLE ZT_BPC_RATES WITH KEY inputcurrency = ls_currency-ID r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' TIME = <ls_time> INTO zs_bpc_rates.
*      fxratetgt_value = zs_bpc_rates-signeddata.
*      CLEAR zs_bpc_rates.

*BPC-3 Insert Start*******************************************************************************
           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_cost_element-ratetype category = <LS_CATEGORY> r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.
*BPC-3 Insert End*********************************************************************************

           <ls_signeddata> = signeddata_kept * ( fxratetgt_value /  fxratesrc_value ).
           <ls_audittrail> = ls_audittrail-rpt_id.
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


           read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

             <ls_signeddata> = signeddata_kept * ( fxratel1_value / fxratesrc_value ).
             <ls_currency> = ls_project-l1currency.
             <ls_audittrail> = ls_audittrail-rpt_id.

             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.


    ENDLOOP.
* Send the result data back. BPC always over-writes existing value. So, send
* the latest values.
    APPEND LINES OF <LT_FINAL> TO CT_DATA.
    "CT_DATA = <LT_FINAL>.


  ENDMETHOD.


  METHOD C_FXTRANS3.

    DATA: BEGIN OF LS_PROJECT,
            ID         TYPE CHAR32,
            L1CURRENCY TYPE CHAR32,
            L1_ID TYPE CHAR32,
            CURRENCY   TYPE CHAR32,
          END OF LS_PROJECT.

    DATA: BEGIN OF LS_COST_ELEMENT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_COST_ELEMENT.

     DATA: BEGIN OF LS_CURRENCY,
            ID        TYPE CHAR32,
            TC_LOOKUP TYPE CHAR32,
          END OF LS_CURRENCY.

    DATA: BEGIN OF LS_AUDITTRAIL,
            ID     TYPE CHAR32,
            RPT_ID TYPE CHAR32,
          END OF LS_AUDITTRAIL.

    DATA: LT_PROJECT      LIKE TABLE OF LS_PROJECT,
          ls_cost_element2 LIKE ls_cost_element,
          LT_COST_ELEMENT LIKE TABLE OF LS_COST_ELEMENT,
          LT_CURRENCY     LIKE TABLE OF LS_CURRENCY,
          LT_AUDITTRAIL   LIKE TABLE OF LS_AUDITTRAIL.


    DATA: LS_SEL              TYPE UJ0_S_SEL,
          LT_SEL              TYPE UJ0_T_SEL,
          ZS_BPC_RATES        TYPE ZBPC_RATES,
          ZT_BPC_RATES        TYPE ZBPC_RATES_T,
          ZS_BPC_PF           TYPE ZBPC_PF,
          ZT_BPC_PF           TYPE ZBPC_PF_T,
          ZS_MARKUP           TYPE ZBPC_PF,
          ZT_MARKUP           TYPE ZBPC_PF_T,
          ZT_BPC_PF_INFLATION TYPE ZBPC_PF_T.

    DATA:  TMPROJECTS  TYPE STANDARD TABLE OF CHAR32,
           TIMEACT     TYPE STANDARD TABLE OF CHAR32,
           TIMECURR    TYPE STRING,
           TIMEFCST    TYPE STANDARD TABLE OF CHAR32,
           TIMEPRIOR   TYPE STRING,
           TIMECURRINP TYPE STRING,
           T_CURR      TYPE STANDARD TABLE OF CHAR32,
           R_CURR      TYPE STANDARD TABLE OF CHAR32,
           ALL_R_CURR  TYPE STANDARD TABLE OF CHAR32,
           CALC_BASE   TYPE STRING,
           TRANSAUDIT  TYPE STANDARD TABLE OF CHAR32.

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


    DATA: COST_EXPENSES               TYPE UJA_T_DIM_MEMBER,
          COST_SALES                  TYPE UJA_T_DIM_MEMBER,
          COST_CONTING                TYPE UJA_T_DIM_MEMBER,
          COST_COST_REC               TYPE UJA_T_DIM_MEMBER,
          COST_LAB_COST               TYPE UJA_T_DIM_MEMBER,
          LINE_ITEM_TOTAL_EXPLINEITEM TYPE UJA_T_DIM_MEMBER.


*****************GET MASTER DATA SELECTIONS*********
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MD
          EXPORTING
            IT_PARAM    = IT_PARAM
          IMPORTING
            TMPROJECTS  = TMPROJECTS
            TIMECURR    = TIMECURR
            TIMEACT     = TIMEACT
            TIMEFCST    = TIMEFCST
            TIMEPRIOR   = TIMEPRIOR
            TIMECURRINP = TIMECURRINP
            T_CURR      = T_CURR
            R_CURR      = R_CURR
            ALL_R_CURR  = ALL_R_CURR
            CALC_BASE   = CALC_BASE
            TRANSAUDIT  = TRANSAUDIT.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
****************************************************


*****************GET COST EXPENSES, SALES, CONTING, COST_REC******************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'EXPENSES'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_EXPENSES.

     TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'LAB_COST'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_LAB_COST.

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'SALES'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_SALES.



    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'CONTING'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_CONTING.




    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'COST_REC'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_COST_REC.


****************************************************

**************** GET LINE_ITEM PARENT ***************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'LINE_ITEM'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'TOTAL_EXPLINEITEM'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = LINE_ITEM_TOTAL_EXPLINEITEM.

****************************************************

*****************GET FULL PROJECT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_PROJECT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'PROJECT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'CURRENCY' TO LT_ATTR_NAME,
            'L1_ID' TO LT_ATTR_NAME,
            'L1CURRENCY' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'PROJECT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_PROJECT.
      APPEND LS_PROJECT TO LT_PROJECT.
    ENDLOOP.

****************************************************

*****************GET FULL COST ELEMENT MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_COST_ELEMENT.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RATETYPE' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'COST_ELEMENT'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_COST_ELEMENT.
      APPEND LS_COST_ELEMENT TO LT_COST_ELEMENT.
    ENDLOOP.

****************************************************

*****************GET FULL CURRENCY MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_CURRENCY.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'CURRENCY'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'TC_LOOKUP' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'CURRENCY'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_CURRENCY.
      APPEND LS_CURRENCY TO LT_CURRENCY.
    ENDLOOP.

****************************************************

*****************GET AUDITTRAIL MD ***************

    REFRESH: LT_ATTR_NAME, LT_SEL, LT_AUDITTRAIL.
    CLEAR: LS_SEL.
    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'AUDITTRAIL'.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
    APPEND: 'ID' TO LT_ATTR_NAME,
            'RPT_ID' TO LT_ATTR_NAME.


    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
    ASSIGN LR_DATA->* TO <LT_MD>.
    LOOP AT <LT_MD> ASSIGNING <LS_MD>.
      MOVE-CORRESPONDING <LS_MD> TO LS_AUDITTRAIL.
      APPEND LS_AUDITTRAIL TO LT_AUDITTRAIL.
    ENDLOOP.

****************************************************


***************** GET INFLATION ********************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_INFLATION
          EXPORTING
            I_APPL_ID    = 'PROJECTFORECAST'
            I_APPSET_ID  = I_APPSET_ID
          IMPORTING
            ET_INFLATION = ZT_BPC_PF_INFLATION.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

****************** GET RATES *************************
    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'Rates'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_RATES    = ZT_BPC_RATES.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

******************************************************
******************************************************
******************************************************
    DATA:  LR_REC          TYPE REF TO DATA,
           LR_RESULT_REC   TYPE REF TO DATA,
           LT_FINAL        TYPE REF TO DATA,
           LINE_ITEM       TYPE CHAR32,
           FXRATETGT_VALUE      TYPE UJ_SDATA,
           FXRATESRC_VALUE      TYPE UJ_SDATA,
           FXRATEL1_VALUE      TYPE UJ_SDATA,
           inflation_value     TYPE UJ_SDATA,
           inflation_time      TYPE string,
           SIGNEDDATA_KEPT TYPE UJ_SDATA,
           CURRENCY_KEPT   TYPE CHAR32,
           AUDITTRAIL_KEPT TYPE CHAR32.

    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <LS_TIME>         TYPE ANY,
                   <LS_CATEGORY>     TYPE ANY,
                   <LS_PROJECT>      TYPE ANY,
                   <LS_CURRENCY>     TYPE ANY,
                   <LS_AUDITTRAIL>   TYPE ANY,
                   <LS_COST_ELEMENT> TYPE ANY,
                   <LS_LINE_ITEM>    TYPE ANY.



    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.

    CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_CURRENCY, ZS_BPC_RATES.
* Loop thro incoming data and create result set
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'LINE_ITEM' OF STRUCTURE <LS_RESULT_REC> TO <LS_LINE_ITEM>.
      ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROJECT>.
      ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CURRENCY>.
      ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <LS_RESULT_REC> TO <LS_COST_ELEMENT>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.


***********************************************************************************
***********************************************************************************
      CLEAR: LS_PROJECT, LS_COST_ELEMENT, LS_COST_ELEMENT2, LS_CURRENCY, LS_AUDITTRAIL, ZS_BPC_PF, ZS_BPC_RATES, FXRATESRC_VALUE, FXRATETGT_VALUE, FXRATEL1_VALUE, LINE_ITEM, inflation_value.

      READ TABLE LINE_ITEM_TOTAL_EXPLINEITEM WITH KEY TABLE_LINE = <LS_LINE_ITEM> INTO LINE_ITEM.
      READ TABLE LT_PROJECT WITH KEY ID = <LS_PROJECT> INTO LS_PROJECT.
      READ TABLE LT_AUDITTRAIL WITH KEY ID = <LS_AUDITTRAIL> INTO LS_AUDITTRAIL.
*
*       read table r_curr with key table_line = <ls_currency> into ls_currency.
*          IF ls_currency is NOT initial.
*              read table lt_currency with key id = ls_currency-id into ls_currency.
*          ENDIF.

        read table cost_expenses with key table_line = <ls_cost_element> into ls_cost_element2.
        read table cost_lab_cost with key table_line = <ls_cost_element> into ls_cost_element2.
          if ls_cost_element2 is NOT initial.
             read table lt_cost_element with key id = ls_cost_element2-id into ls_cost_element2.
          ENDIF.

         IF LS_COST_ELEMENT2 IS INITIAL.

         read table cost_sales with key table_line = <ls_cost_element> into ls_cost_element.
         read table cost_conting with key table_line = <ls_cost_element> into ls_cost_element.
         read table cost_cost_rec with key table_line = <ls_cost_element> into ls_cost_element.

         if ls_cost_element is NOT initial.
             read table lt_cost_element with key id = ls_cost_element-id into ls_cost_element.
         ENDIF.

         READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratesrc_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratel1_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

         ELSE.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-currency r_account = ls_cost_element2-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratesrc_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

          READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_project-l1currency r_account = ls_cost_element2-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
          fxratel1_value = zs_bpc_rates-signeddata.

           clear zs_bpc_rates.

         ENDIF.

      CONCATENATE <ls_time>(5) 'INP' INTO inflation_time.
      SIGNEDDATA_KEPT = <LS_SIGNEDDATA>.
      CURRENCY_KEPT = <LS_CURRENCY>.
      AUDITTRAIL_KEPT = <LS_AUDITTRAIL>.
***********************************************************************************
***********************************************************************************



IF <LS_LINE_ITEM> = 'NO_LINEITEM'.


     IF <ls_cost_element> = ls_cost_element-id.

         LOOP AT r_curr INTO ls_currency-id.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_cost_element-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.


           <ls_signeddata> = signeddata_kept * ( fxratetgt_value /  fxratesrc_value ).
           <ls_audittrail> = ls_audittrail-rpt_id.
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


           read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

             <ls_signeddata> = signeddata_kept * ( fxratel1_value / fxratesrc_value ).
             <ls_currency> = ls_project-l1currency.
             <ls_audittrail> = ls_audittrail-rpt_id.

             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.


      ELSEIF <ls_cost_element> = ls_cost_element2-id.


           READ TABLE ZT_BPC_PF_INFLATION WITH key project = ls_project-l1_id time = inflation_time INTO ZS_BPC_PF.
           inflation_value = ZS_BPC_PF-signeddata.
           clear ZS_BPC_PF.

           "************************
            if inflation_value = 0.
              inflation_value = 1.
            endif.
           "*************************


          LOOP AT r_curr INTO ls_currency-id.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_cost_element2-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.

           <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratetgt_value /  fxratesrc_value ) ).
           <ls_audittrail> = ls_audittrail-rpt_id.
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


          read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

            <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratel1_value / fxratesrc_value ) ).
             <ls_currency> = ls_project-l1currency.
             <ls_audittrail> = ls_audittrail-rpt_id.

             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.


       ENDIF.


ELSEIF <LS_LINE_ITEM> = LINE_ITEM.



     IF <ls_cost_element> = ls_cost_element2-id OR <ls_cost_element> = 'CE_601000'.


           READ TABLE ZT_BPC_PF_INFLATION WITH key project = ls_project-l1_id time = inflation_time INTO ZS_BPC_PF.
           inflation_value = ZS_BPC_PF-signeddata.
           clear ZS_BPC_PF.

           "************************
            if inflation_value = 0.
              inflation_value = 1.
            endif.
           "*************************

          LOOP AT r_curr INTO ls_currency-id.

           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = 'AVG' category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.

           <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratetgt_value /  fxratesrc_value ) ).
           <ls_audittrail> = ls_audittrail-rpt_id.
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


          read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

            <ls_signeddata> = signeddata_kept * ( inflation_value * ( fxratel1_value / fxratesrc_value ) ).
             <ls_currency> = ls_project-l1currency.
             <ls_audittrail> = ls_audittrail-rpt_id.

             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.

        ENDIF.



ELSE.

 IF <ls_cost_element> = ls_cost_element2-id OR <ls_cost_element> = 'CE_601000'.

         LOOP AT r_curr INTO ls_currency-id.

 IF <ls_cost_element> = 'CE_601000'.
           READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = 'AVG'  category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.
 ELSE.

      READ TABLE ZT_BPC_RATES WITH key inputcurrency = ls_currency-id r_account = ls_cost_element2-ratetype category = 'WFORECAST' r_entity = 'Global' time = <ls_time> INTO zs_bpc_rates.
           fxratetgt_value = zs_bpc_rates-signeddata.
           clear zs_bpc_rates.


 ENDIF.


           <ls_signeddata> = signeddata_kept * ( fxratetgt_value /  fxratesrc_value ).
           <ls_audittrail> = ls_audittrail-rpt_id.
           <ls_currency> = ls_currency-id.

           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           clear: fxratetgt_value, ls_currency.

           ENDLOOP.


           read table r_curr with key table_line = ls_project-l1currency TRANSPORTING NO FIELDS.

           IF sy-subrc <> 0.

             <ls_signeddata> = signeddata_kept * ( fxratel1_value / fxratesrc_value ).
             <ls_currency> = ls_project-l1currency.
             <ls_audittrail> = ls_audittrail-rpt_id.

             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           ENDIF.
      ENDIF.


ENDIF.



    ENDLOOP.
* Send the result data back. BPC always over-writes existing value. So, send
* the latest values.
    APPEND LINES OF <LT_FINAL> TO CT_DATA.
    "CT_DATA = <LT_FINAL>.


  ENDMETHOD.


  METHOD C_WARSTATUSCHANGE.

    DATA: BEGIN OF LS_COST_ELEMENT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_COST_ELEMENT.



    DATA:  ls_cost_element_sales LIKE ls_cost_element,
           ls_cost_element_costs LIKE ls_cost_element.



    DATA: LS_SEL              TYPE UJ0_S_SEL,
          LT_SEL              TYPE UJ0_T_SEL,
          ZS_WARRAG           TYPE ZBPC_PF,
          ZT_WARRAG           TYPE ZBPC_PF_T.


    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
          LR_DATA      TYPE REF TO DATA,
          LS_MD        TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_MD> TYPE STANDARD TABLE,
                   <LS_MD> TYPE ANY.


    DATA: COST_SALES TYPE UJA_T_DIM_MEMBER,
          COST_COSTS TYPE UJA_T_DIM_MEMBER.

*****************GET COST PARENTS******************

    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'SALES'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_SALES.



    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = 'COST_ELEMENT'.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = 'COSTS'
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = COST_COSTS.
****************************************************


****************** GET WAR RAG FLAG *************************
clear: ls_sel, lt_sel.

ls_sel-attribute = 'ID'.
ls_sel-sign = 'I'.
ls_sel-option = 'EQ'.

ls_sel-dimension = 'AUDITTRAIL'.
ls_sel-low = 'INPUT'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'COST_ELEMENT'.
ls_sel-low = 'CE_1002'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'CATEGORY'.
ls_sel-low = 'WFORECAST'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'CURRENCY'.
ls_sel-low = 'LC'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'LINE_ITEM'.
ls_sel-low = 'NO_LINEITEM'.
APPEND ls_sel to lt_sel.

    TRY.
        CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
          EXPORTING
            LT_SEL      = LT_SEL
            I_APPL_ID   = 'PROJECTFORECAST'
            I_APPSET_ID = I_APPSET_ID
          IMPORTING
            ET_PF    = ZT_WARRAG.
      CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************


******************************************************
******************************************************
******************************************************
    DATA:  LR_REC        TYPE REF TO DATA,
           LR_RESULT_REC TYPE REF TO DATA,
           LT_FINAL      TYPE REF TO DATA.


    FIELD-SYMBOLS: <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY,
                   <LS_SIGNEDDATA> TYPE ANY,
                   <LT_FINAL>      TYPE STANDARD TABLE.

    FIELD-SYMBOLS: <ls_time> TYPE ANY,
                   <ls_category> TYPE ANY,
                   <ls_project> TYPE ANY,
                   <ls_currency> TYPE ANY,
                   <ls_audittrail> TYPE ANY,
                   <ls_cost_element> TYPE ANY.



    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LR_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LR_REC LIKE LINE OF CT_DATA.
    ASSIGN LR_REC->* TO <LS_REC>.


* Loop thro incoming data and create result set
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.
    ASSIGN COMPONENT 'TIME' OF STRUCTURE <ls_result_rec> to <ls_time>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <ls_result_rec> to <ls_category>.
    ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <ls_result_rec> to <ls_audittrail>.
    ASSIGN COMPONENT 'PROJECT' OF STRUCTURE <ls_result_rec> to <ls_project>.
    ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <ls_result_rec> to <ls_currency>.
    ASSIGN COMPONENT 'COST_ELEMENT' OF STRUCTURE <ls_result_rec> TO <ls_cost_element>.
    ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <ls_result_rec> to <ls_signeddata>.

    clear: ls_cost_element_sales, ls_cost_element_costs, zs_warrag.

    read table cost_sales with key table_line = <ls_cost_element> INTO ls_cost_element_sales-id.

    read table cost_costs with key table_line = <ls_cost_element> INTO  ls_cost_element_costs-id.

    READ TABLE ZT_WARRAG with key project = <ls_project> time = <ls_time> INTO zs_warrag.

   IF ls_cost_element_sales IS NOT INITIAL.

     IF zs_warrag-signeddata = 1 or zs_warrag-signeddata = 2.

     <ls_signeddata> = <ls_signeddata> * -1.
     <ls_audittrail> = 'WAR_ADJ'.

      APPEND <LS_RESULT_REC> TO <LT_FINAL>.

     ENDIF.

   ELSEIF ls_cost_element_costs IS NOT INITIAL.

     IF zs_warrag-signeddata = 2.
     <ls_signeddata> = <ls_signeddata> * -1.
     <ls_audittrail> = 'WAR_ADJ'.

     APPEND <LS_RESULT_REC> TO <LT_FINAL>.

     ENDIF.

 ENDIF.


    ENDLOOP.
* Send the result data back. BPC always over-writes existing value. So, send
* the latest values.

    APPEND LINES OF <lt_final> to ct_data.
    "CT_DATA = <LT_FINAL>.


  ENDMETHOD.


  method GET_INFLATION.

**************************** READ MODEL DATA **********************************
*******************************************************************************
DATA:  lv_environment_id TYPE uj_appset_id,
       lv_application_id TYPE uj_appl_id,
       ls_sel TYPE uj0_s_sel,
       lt_sel TYPE uj0_t_sel,
       lt_dim_list TYPE uja_t_dim_list,
       lo_appl_mgr TYPE REF TO if_uja_application_manager,
       lo_query TYPE REF TO if_ujo_query,
        lr_data TYPE REF TO data,
       ls_application type UJA_S_APPLICATION,
       ls_dimensions type UJA_s_DIMENSION,
       ls_inflation type ZBPC_PF.


lv_environment_id = i_appset_id.
lv_application_id = i_appl_id.


FIELD-SYMBOLS: <lt_query_result> TYPE STANDARD TABLE,
               <ls_query_result> TYPE ANY.


lo_appl_mgr = cl_uja_bpc_admin_factory=>get_application_manager(
                 i_appset_id      =  lv_environment_id
                 i_application_id =  lv_application_id ).


clear ls_application.
lo_appl_mgr->GET(
  exporting
    IF_WITH_MEASURES = ABAP_FALSE    " BPC: Generic indicator
    IF_SUMMARY       = ABAP_FALSE    " BPC: Generic indicator
  importing
    ES_APPLICATION   = ls_application ).  " Applications table type

refresh lt_dim_list.
loop at ls_application-dimensions into ls_dimensions.
  append ls_dimensions-dimension to lt_dim_list.
endloop.


lo_appl_mgr->create_data_ref(
  EXPORTING
    i_data_type   = 'T'
    it_dim_name   = lt_dim_list
    if_tech_name  = abap_false
    if_signeddata = abap_true
  IMPORTING
    er_data       = lr_data ).


ASSIGN lr_data->* TO <lt_query_result>.



ls_sel-attribute = 'ID'.
ls_sel-sign = 'I'.
ls_sel-option = 'EQ'.

ls_sel-dimension = 'COST_ELEMENT'.
ls_sel-low = 'CE_1032'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'LINE_ITEM'.
ls_sel-low = 'NO_LINEITEM'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'CURRENCY'.
ls_sel-low = 'LC'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'AUDITTRAIL'.
ls_sel-low = 'INPUT'.
APPEND ls_sel to lt_sel.

ls_sel-dimension = 'CATEGORY'.
ls_sel-low = 'WFORECAST'.
APPEND ls_sel to lt_sel.


TRY.
    lo_query = cl_ujo_query_factory=>get_query_adapter(
        i_appset_id = lv_environment_id
        i_appl_id   = lv_application_id
    ).


    lo_query->run_rsdri_query(
      EXPORTING
        it_dim_name       =  lt_dim_list   " BPC: Dimension List
        it_range          =  lt_sel   " BPC: Selection condition
         if_check_security = ABAP_FALSE    " BPC: Generic indicator

       IMPORTING
         et_data           = <lt_query_result>
*       et_message        = lt_message    " BPC: Messages
    ).
*      CATCH cx_ujo_read.    " Exception of common read


  CATCH cx_ujo_read.  " Exception of common read
ENDTRY.
**************************** END READ MODEL DATA ******************************
*******************************************************************************

LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
  MOVE-CORRESPONDING <ls_query_result> to ls_inflation.
  APPEND ls_inflation to et_inflation.
ENDLOOP.


  endmethod.


 METHOD GET_MD.

DATA:   ls_param TYPE ujk_s_script_logic_hashentry,
        lv_param TYPE char20,
        l_log    TYPE string,
        ls_message TYPE uj0_s_message,
        lt_message TYPE uj0_t_message.


CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TMPROJECTS' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE TMPROJECTS.
IF sy-subrc <> 0.
l_log = 'TMPROJECTS are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TIMEACT' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE TIMEACT.
IF sy-subrc <> 0.
l_log = 'TIMEACT are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TIMECURR' INTO ls_param.
TIMECURR = ls_param-hashvalue.
IF sy-subrc <> 0.
l_log = 'TIMECURR are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TIMEFCST' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE TIMEFCST.
IF sy-subrc <> 0.
l_log = 'TIMEFCST are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TIMEPRIOR' INTO ls_param.
TIMEPRIOR = ls_param-hashvalue.
IF sy-subrc <> 0.
l_log = 'TIMEPRIOR are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TIMECURRINP' INTO ls_param.
TIMECURRINP = ls_param-hashvalue.
IF sy-subrc <> 0.
l_log = 'TIMECURRINP are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'T_CURR' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE T_CURR.
IF sy-subrc <> 0.
l_log = 'T_CURR are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'R_CURR' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE R_CURR.
IF sy-subrc <> 0.
l_log = 'R_CURR are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'ALL_R_CURR' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE ALL_R_CURR.
IF sy-subrc <> 0.
l_log = 'ALL_R_CURR are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'CALC_BASE' INTO ls_param.
CALC_BASE = ls_param-hashvalue.
IF sy-subrc <> 0.
l_log = 'CALC_BASE are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CLEAR: ls_param.
READ TABLE it_param WITH KEY hashkey = 'TRANSAUDIT' INTO ls_param.
SPLIT ls_param-hashvalue AT ',' INTO TABLE TRANSAUDIT.
IF sy-subrc <> 0.
l_log = 'TRANSAUDIT are missing in script logic'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.



ENDMETHOD.


  method GET_MODEL_DATA.

**************************** READ MODEL DATA **********************************
*******************************************************************************
DATA:  lv_environment_id TYPE uj_appset_id,
       lv_application_id TYPE uj_appl_id,
       lt_dim_list TYPE uja_t_dim_list,
       lo_appl_mgr TYPE REF TO if_uja_application_manager,
       lo_query TYPE REF TO if_ujo_query,
        lr_data TYPE REF TO data,
       ls_application type UJA_S_APPLICATION,
       ls_dimensions type UJA_s_DIMENSION,
       ls_rates type ZBPC_RATES,
       ls_pf type ZBPC_PF,
       ls_rd type ZBPC_RD.

lv_environment_id = i_appset_id.
lv_application_id = i_appl_id.


FIELD-SYMBOLS: <lt_query_result> TYPE STANDARD TABLE,
               <ls_query_result> TYPE ANY.


lo_appl_mgr = cl_uja_bpc_admin_factory=>get_application_manager(
                 i_appset_id      =  lv_environment_id
                 i_application_id =  lv_application_id ).


clear ls_application.
lo_appl_mgr->GET(
  exporting
    IF_WITH_MEASURES = ABAP_FALSE    " BPC: Generic indicator
    IF_SUMMARY       = ABAP_FALSE    " BPC: Generic indicator
  importing
    ES_APPLICATION   = ls_application ).  " Applications table type

refresh lt_dim_list.
loop at ls_application-dimensions into ls_dimensions.
  append ls_dimensions-dimension to lt_dim_list.
endloop.


lo_appl_mgr->create_data_ref(
  EXPORTING
    i_data_type   = 'T'
    it_dim_name   = lt_dim_list
    if_tech_name  = abap_false
    if_signeddata = abap_true
  IMPORTING
    er_data       = lr_data ).


ASSIGN lr_data->* TO <lt_query_result>.


TRY.
    lo_query = cl_ujo_query_factory=>get_query_adapter(
        i_appset_id = lv_environment_id
        i_appl_id   = lv_application_id
    ).


    lo_query->run_rsdri_query(
      EXPORTING
        it_dim_name       =  lt_dim_list   " BPC: Dimension List
        it_range          =  lt_sel   " BPC: Selection condition
         if_check_security = ABAP_FALSE    " BPC: Generic indicator

       IMPORTING
         et_data           = <lt_query_result>
*       et_message        = lt_message    " BPC: Messages
    ).
*      CATCH cx_ujo_read.    " Exception of common read


  CATCH cx_ujo_read.  " Exception of common read
ENDTRY.
**************************** END READ MODEL DATA ******************************
*******************************************************************************
CASE i_appl_id.

WHEN 'Rates'.
LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
  MOVE-CORRESPONDING <ls_query_result> to ls_rates.
  APPEND ls_rates to et_rates.
ENDLOOP.

WHEN 'PROJECTFORECAST'.
LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
  MOVE-CORRESPONDING <ls_query_result> to ls_pf.
  APPEND ls_pf to et_pf.
ENDLOOP.

ENDCASE.


  endmethod.


  method IF_UJ_CUSTOM_LOGIC~CLEANUP.
  endmethod.


  method IF_UJ_CUSTOM_LOGIC~EXECUTE.

 DATA: ls_sel TYPE uj0_s_sel,
       lt_sel TYPE uj0_t_sel,
       ZS_BPC_RATES type ZBPC_RATES,
       ZT_BPC_RATES TYPE ZBPC_RATES_T,
       ZS_BPC_PF type ZBPC_PF,
       ZT_BPC_PF TYPE ZBPC_PF_T,
       ZT_BPC_PF_INFLATION TYPE ZBPC_PF_T.

DATA:  TMPROJECTS TYPE STANDARD TABLE OF CHAR32,
       TIMEACT TYPE STANDARD TABLE OF CHAR32,
       TIMECURR TYPE STRING,
       TIMEFCST TYPE STANDARD TABLE OF CHAR32,
       TIMEPRIOR TYPE STRING,
       TIMECURRINP TYPE STRING,
       T_CURR TYPE STANDARD TABLE OF CHAR32,
       R_CURR TYPE STANDARD TABLE OF CHAR32,
       ALL_R_CURR TYPE STANDARD TABLE OF CHAR32,
       CALC_BASE TYPE STRING,
       TRANSAUDIT TYPE STANDARD TABLE OF CHAR32.

DATA:   ls_param TYPE ujk_s_script_logic_hashentry,
        lv_param TYPE char20,
        caller_id TYPE string,
        l_log    TYPE string,
        ls_message TYPE uj0_s_message,
        lt_message TYPE uj0_t_message.


CLEAR: ls_param, et_message.
READ TABLE it_param WITH KEY hashkey = 'CALLER_ID' INTO ls_param.
caller_id = ls_param-hashvalue.
IF sy-subrc <> 0.
l_log = 'The script logic CALLER_ID parameter should be specified!'.
cl_ujk_logger=>log( i_object = l_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

CASE caller_id.

WHEN 'CLEAR'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>CLEAR_DATA
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.



WHEN 'C_CURRTRANS'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_CURRTRANS
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.



WHEN 'C_CURRTRANS_RD'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_CURRTRANS_RD
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.


WHEN 'C_FXTRANS'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_FXTRANS
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.


WHEN 'C_FXTRANS3'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_FXTRANS3
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.

WHEN 'C_WARSTATUSCHANGE'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_WARSTATUSCHANGE
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.


WHEN 'C_FCSTSNAPSHOT'.

TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>C_FCSTSNAPSHOT
      EXPORTING
        it_param = it_param
        it_cv = it_cv
        i_appl_id =  i_appl_id
        i_appset_id = i_appset_id
      CHANGING
        ct_data = ct_data.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.


WHEN 'DEFAULT'.

*****************GET MASTER DATA SELECTIONS*********
TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MD
      EXPORTING
        it_param = it_param
      IMPORTING
        TMPROJECTS = TMPROJECTS
        TIMECURR = TIMECURR
        TIMEACT = TIMEACT
        TIMEFCST = TIMEFCST
        TIMEPRIOR = TIMEPRIOR
        TIMECURRINP = TIMECURRINP
        T_CURR = T_CURR
        R_CURR = R_CURR
        ALL_R_CURR = ALL_R_CURR
        CALC_BASE = CALC_BASE
        TRANSAUDIT = TRANSAUDIT.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
****************************************************

***************** GET INFLATION ********************
TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_INFLATION
      EXPORTING
        i_appl_id = 'PROJECTFORECAST'
        i_appset_id = i_appset_id

      IMPORTING
        ET_INFLATION = ZT_BPC_PF_INFLATION.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

****************** GET RATES *************************
TRY.
      CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
      EXPORTING
        lt_sel = lt_sel
        i_appl_id = 'Rates'
        i_appset_id = i_appset_id

      IMPORTING
        ET_RATES = ZT_BPC_RATES.
    CATCH CX_UJ_CUSTOM_LOGIC.
    ENDTRY.
******************************************************

*CASE caller_id.
*
*WHEN 'INFLATION'.
*    TRY.
*      CALL METHOD ZCL_BPC_COMPLEX_BADI=>GET_MODEL_DATA
*      EXPORTING
*        lt_sel = lt_sel
*        i_appl_id = 'PROJECTFORECAST'
*        i_appset_id = i_appset_id
*
*      IMPORTING
*        ET_PF = ZT_BPC_PF_INFLATION.
*    CATCH CX_UJ_CUSTOM_LOGIC.
*    ENDTRY.
*
ENDCASE.

  endmethod.


  method IF_UJ_CUSTOM_LOGIC~INIT.
  endmethod.
ENDCLASS.