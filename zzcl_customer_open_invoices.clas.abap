CLASS ZZCL_CUSTOMER_OPEN_INVOICES DEFINITION
  PUBLIC

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: IF_AMDP_MARKER_HDB.
    TYPES:
      TT_CUST_INFO TYPE STANDARD TABLE OF
                     ZZCUSTOMER_INFO WITH KEY CUSTOMER_ID,
      TT_INV_INFO  TYPE STANDARD TABLE OF
       ZZINVOICE_INFO WITH KEY CUSTOMER_ID.
    METHODS:
      GET_CUSTOMER_INFO
        IMPORTING
          VALUE(IV_CLIENT) TYPE SYMANDT
          VALUE(IV_BUPAID)
            TYPE ZZCUSTOMER_INFO-CUSTOMER_ID
        EXPORTING
          VALUE(ET_BPINFO) TYPE TT_CUST_INFO,
      GET_INVOICE_INFO
        IMPORTING
          VALUE(IV_CLIENT)  TYPE SYMANDT
          VALUE(IV_BUPAID)
            TYPE ZZCUSTOMER_INFO-CUSTOMER_ID
        EXPORTING
          VALUE(ET_INVINFO) TYPE TT_INV_INFO.



  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF TY_REL_ITEMS,
        CLIENT             TYPE SNWD_SO_INV_ITEM-CLIENT,
        INV_I_GUID         TYPE SNWD_SO_INV_ITEM-NODE_KEY,
        INV_GUID           TYPE SNWD_SO_INV_HEAD-NODE_KEY,
        BUYER_GUID         TYPE SNWD_BPA-NODE_KEY,
        CUSTOMER_ID        TYPE SNWD_BPA-BP_ID,
        INVOICE_DATE       TYPE SNWD_SO_INV_HEAD-CREATED_AT,
        GROSS_AMOUNT       TYPE SNWD_SO_INV_ITEM-GROSS_AMOUNT,
        CURRENCY_CODE_CONV
                             TYPE SNWD_SO_INV_ITEM-CURRENCY_CODE,
      END OF TY_REL_ITEMS,
      TT_REL_ITEMS TYPE
                     STANDARD TABLE OF TY_REL_ITEMS.

    METHODS:
      GET_CURR_CONV_RELEVANT_ITEMS
        IMPORTING
          VALUE(IV_CLIENT)     TYPE SYMANDT
          VALUE(IV_BUPAID)
            TYPE ZZCUSTOMER_INFO-CUSTOMER_ID
        EXPORTING
          VALUE(ET_CONV_ITEMS) TYPE TT_REL_ITEMS.


ENDCLASS.



CLASS ZZCL_CUSTOMER_OPEN_INVOICES IMPLEMENTATION.


  METHOD GET_CUSTOMER_INFO BY DATABASE PROCEDURE
 FOR HDB
 LANGUAGE SQLSCRIPT
 OPTIONS READ-ONLY
 USING SNWD_BPA SNWD_AD
 ZZCL_CUSTOMER_OPEN_INVOICES=>GET_CURR_CONV_RELEVANT_ITEMS.

    call
 "ZZCL_CUSTOMER_OPEN_INVOICES=>GET_CURR_CONV_RELEVANT_ITEMS" (
  iv_client => :iv_client,
  iv_bupaid => :iv_bupaid,
  et_conv_items => :lt_converted_items );
  --aggregated gross amounts per customer
  et_bpinfo =
  select
  customer_id,
  bpa.company_name as customer_name,
  ad.city,
  ad.street,
  ad.postal_code,
  ad.country,
  conv_items.currency_code_conv as currency_code,
  sum( conv_items.gross_amount ) as sum_gross_amount
  from :lt_converted_items as conv_items
  join snwd_bpa as bpa
  on conv_items.client = bpa.client
  and buyer_guid = bpa.node_key
  join snwd_ad as ad
  on bpa.client = ad.client
  and bpa.address_guid = ad.node_key
  group by customer_id, company_name, city, street,
  postal_code, country, currency_code_conv;


  ENDMETHOD.

  METHOD GET_INVOICE_INFO BY DATABASE PROCEDURE

 FOR HDB
 LANGUAGE SQLSCRIPT
 OPTIONS READ-ONLY
 USING SNWD_SO SNWD_SO_INV_HEAD
ZZCL_CUSTOMER_OPEN_INVOICES=>GET_CURR_CONV_RELEVANT_ITEMS.

    call
  "ZZCL_CUSTOMER_OPEN_INVOICES=>GET_CURR_CONV_RELEVANT_ITEMS"
  (
   iv_client => :iv_client,
   iv_bupaid => :iv_bupaid,
   et_conv_items => :lt_converted_items );
   --aggregated gross amounts per sales order invoice
   et_invinfo =
   select
   customer_id,
   so_id as order_id,
   invoice_date,
   currency_code_conv as currency_code,
   sum( conv_items.gross_amount )
   as sum_gross_amount
   from :lt_converted_items as conv_items
   join snwd_so_inv_head as h
   on h.client = conv_items.client
   and h.node_key = conv_items.inv_guid
   join snwd_so as so
   on so.client = h.client
   and so.node_key = h.so_guid
   group by customer_id, so_id, invoice_date,
   currency_code_conv
   order by so_id asc;


  ENDMETHOD.

  METHOD GET_CURR_CONV_RELEVANT_ITEMS  BY DATABASE PROCEDURE
 FOR HDB
 LANGUAGE SQLSCRIPT
 OPTIONS READ-ONLY

 USING SNWD_BPA SNWD_SO_INV_HEAD SNWD_SO_INV_ITEM.

    -- declare a local variable
    declare lv_today date;
    -- get current date for conversion
    select current_date into lv_today from dummy;
    -- select relevant invoice items
    lt_relevant_items =
    select
    i.client as client,
    i.node_key as inv_i_guid,
    h.node_key as inv_guid,
    bpa.node_key as buyer_guid,
    bpa.bp_id as customer_id,
    h.created_at as invoice_date,
    i.gross_amount,
    i.currency_code
    from snwd_so_inv_item as i
    join snwd_so_inv_head as h
    on i.client = h.client
    and i.parent_key = h.node_key
    join snwd_bpa as bpa
    on h.client = bpa.client
    and h.buyer_guid = bpa.node_key
    where h.client = :iv_client
    and bpa.bp_id = :iv_bupaid
    and h.payment_status = '';
    --convert gross amount of items to currency 'USD'
    et_conv_items =
    ce_conversion( :lt_relevant_items,
    [ family = 'currency',
    method = 'ERP',
    steps =
   'shift,convert,shift_back',
    source_unit_column =
   "CURRENCY_CODE" ,
    output_unit_column =
   "CURRENCY_CODE_CONV",
    target_unit = 'USD',
    reference_date = :lv_today,
    client = :iv_client
   ],
    [gross_amount] ) ;


  ENDMETHOD.

ENDCLASS.
