@AbapCatalog.sqlViewName: 'ZZV_CDS_INVC'
define view zzcdsv_open_invoice as select from snwd_so_inv_head
{
 key snwd_so_inv_head.buyer_guid,
 'C' as category
}
where snwd_so_inv_head.payment_status <> 'P'
group by snwd_so_inv_head.buyer_guid
having count( distinct snwd_so_inv_head.node_key ) <= 2000
union all 
select from snwd_so_inv_head
{
 key snwd_so_inv_head.buyer_guid,
 'D' as category
}
where snwd_so_inv_head.payment_status <> 'P'
group by snwd_so_inv_head.buyer_guid
having count( distinct snwd_so_inv_head.node_key ) > 2000
 and count( distinct snwd_so_inv_head.node_key ) <= 4000
union all
select from snwd_so_inv_head
{
 key snwd_so_inv_head.buyer_guid,
 'S' as category
}
where snwd_so_inv_head.payment_status <> 'P'
group by snwd_so_inv_head.buyer_guid
having count( distinct snwd_so_inv_head.node_key ) > 4000