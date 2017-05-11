CLASS ZZCL_ZZE2E_CUST_INFO_DPC_EXT DEFINITION
  PUBLIC
  INHERITING FROM ZZCL_ZZE2E_CUST_INFO_DPC
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS INVOICEINFOS_GET_ENTITYSET REDEFINITION.
    METHODS CUSTOMERINFOS_GET_ENTITY REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_ZZE2E_CUST_INFO_DPC_EXT IMPLEMENTATION.

  METHOD INVOICEINFOS_GET_ENTITYSET.

    "Entityset can only be accessed via the navigation property ToInvoiceInfo
    "of entity type CustomerClassification and if the key CustomerID is provided
    IF IV_SOURCE_NAME <> ZZCL_ZZE2E_CUST_INFO_MPC=>GC_CUSTOMERCLASSIFICATION.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_TECH_EXCEPTION.
    ENDIF.
    READ TABLE IT_KEY_TAB INTO DATA(LS_KEY) WITH KEY NAME = 'CustomerId' ##NO_TEXT.
    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_BUSI_EXCEPTION
        EXPORTING
          TEXTID = /IWBEP/CX_MGW_BUSI_EXCEPTION=>FILTER_NOT_SUPPORTED.
    ENDIF.
    DATA LV_CUSTOMER_ID_FILTER TYPE SNWD_PARTNER_ID.
    "Apply the conversion of snwd_partner_id (domain D_EPM_ID)
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LS_KEY-VALUE
      IMPORTING
        OUTPUT = LV_CUSTOMER_ID_FILTER.
    "create an instance of class zcl_customer_open_invoices
    DATA(LO_CUST_INFO) = NEW ZZCL_CUSTOMER_OPEN_INVOICES( ).
    "call the ABAP managed DB procedure respectively the
    "ABAP class method get_invoice_info
    LO_CUST_INFO->GET_INVOICE_INFO(

    EXPORTING

    IV_CLIENT = SY-MANDT
    IV_BUPAID = LV_CUSTOMER_ID_FILTER
    IMPORTING
    ET_INVINFO = DATA(LT_ENTITYSET)
    ).
    "provide the resultset of the AMDP to the tabular
    "tabular output parameter
    ET_ENTITYSET = LT_ENTITYSET.
  ENDMETHOD.


  METHOD CUSTOMERINFOS_GET_ENTITY.

    "Entity can only be accessed via the navigation property ToCustomerInfo
    "of entity type CustomerClassification and if the key CustomerID is provided
    IF IV_SOURCE_NAME <> ZZCL_ZZE2E_CUST_INFO_MPC=>GC_CUSTOMERCLASSIFICATION.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_TECH_EXCEPTION.
    ENDIF.
    READ TABLE IT_KEY_TAB INTO DATA(LS_KEY) WITH KEY NAME = 'CustomerId' ##NO_TEXT.
    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_BUSI_EXCEPTION
        EXPORTING
          TEXTID = /IWBEP/CX_MGW_BUSI_EXCEPTION=>FILTER_NOT_SUPPORTED.
    ENDIF.
    DATA LV_CUSTOMER_ID_FILTER TYPE SNWD_PARTNER_ID.
    "Apply the conversion of snwd_partner_id (domain D_EPM_ID)
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LS_KEY-VALUE
      IMPORTING
        OUTPUT = LV_CUSTOMER_ID_FILTER.
    "create an instance of class zcl_customer_open_invoices
    DATA(LO_CUST_INFO) = NEW ZZCL_CUSTOMER_OPEN_INVOICES( ).
    "call the ABAP managed DB procedure respectively the
    "ABAP class method get_customer_info
    LO_CUST_INFO->GET_CUSTOMER_INFO(
    EXPORTING
    IV_CLIENT = SY-MANDT
    IV_BUPAID = LV_CUSTOMER_ID_FILTER
    IMPORTING
    ET_BPINFO = DATA(LT_ENTITY)
    ).
    "only one line is expected to be retrieved
    IF LINES( LT_ENTITY ) <> 1.
      RAISE EXCEPTION TYPE /IWBEP/CX_MGW_BUSI_EXCEPTION
        EXPORTING
          MESSAGE_UNLIMITED = | Problem in Customer Info; { LINES( LT_ENTITY ) } <> 1 |
          TEXTID            = /IWBEP/CX_MGW_BUSI_EXCEPTION=>BUSINESS_ERROR_UNLIMITED.
    ENDIF.
    "map the first row of lt_entity to the output
    ER_ENTITY = LT_ENTITY[ 1 ].

  ENDMETHOD.

ENDCLASS.
