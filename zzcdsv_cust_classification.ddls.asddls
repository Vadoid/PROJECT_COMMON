@AbapCatalog.sqlViewName: 'ZZV_CDS_CUST'
define view zzcdsv_cust_classification as select from snwd_bpa as bpa
join zzcdsv_open_invoice as opn_inv on bpa.node_key = opn_inv.buyer_guid
{
 key bpa.bp_id as customer_id,
 key bpa.company_name as customer_name,
 opn_inv.category
}
