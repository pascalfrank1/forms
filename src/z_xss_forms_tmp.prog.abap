*&---------------------------------------------------------------------*
*& Report z_xss_forms_tmp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_xss_forms_tmp.
*data aa type i.
*data(xx) = 1.
*
*data(yy) = aa  + xx.
*
delete from zform_headers.
delete from zform_mob_work.
delete from zform_notes.
return.
*data abc type boolean.



TYPES: Begin of json_header_items,
       name TYPE string,
       category TYPE string,
       value TYPE string,
       type TYPE string,
       page TYPE char30,
       confidence TYPE char30,
       label TYPE string,
       END OF json_header_items.
TYPES: t_header TYPE STANDARD TABLE OF json_header_items WITH DEFAULT KEY.


TYPES: BEGIN OF json_final,
       header_Fields Type t_header,
       END OF json_final.

TYPES: t_json_final TYPE STANDARD TABLE OF JSON_final WITH DEFAULT KEY.


DATA: json TYPE string VALUE '{"headerFields":[{"name":"receiverName","category":"receiver","value":"Kiekert CS s.r.o.","rawValue":null,"type":"string","page":1,"confidence":1,"label":"receiverName"}]}'.
DATA(it_json) = VALUE json_final( ).


/ui2/cl_json=>deserialize( EXPORTING json = json pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = it_json ).
"CALL TRANSFORMATION id SOURCE XML json RESULT values = it_json.
cl_demo_output=>display_data( it_json ).
