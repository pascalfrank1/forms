CLASS zcl_zform_mobile_work_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zform_mobile_work_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
methods: /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity REDEFINITION.

  PROTECTED SECTION.
    METHODS: dataset_create_entity REDEFINITION,
            dataset_get_entity REDEFINITION,
            overviewset_get_entityset REDEFINITION,
            substitutes_get_entityset REDEFINITION,
            substitutes_get_entity REDEFINITION.

  PRIVATE SECTION.

data: current_substitute type pernr_d.

ENDCLASS.



CLASS zcl_zform_mobile_work_dpc_ext IMPLEMENTATION.
  METHOD dataset_create_entity.

    DATA: request_data TYPE zcl_zform_mobile_work_mpc_ext=>ts_data,
          table_data   TYPE zform_mob_work,
          header       TYPE zform_headers.

    io_data_provider->read_entry_data( IMPORTING es_data = request_data ).


    DATA(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).
    DATA(header_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).

    table_data = CORRESPONDING #( request_data ).
    table_data-employee_number = current_employee.

    table_data-header_id = header_id.



    GET TIME STAMP FIELD DATA(current_timestamp).

    header = VALUE zform_headers(
        mandt           = sy-mandt
        id              = header_id
        employee_number = current_employee
        application     = 'MOB_WORK'
        timestamp       = current_timestamp
        approval_step   = 0
    ).


    INSERT zform_headers FROM header.

    IF sy-subrc = 0.
      INSERT zform_mob_work FROM table_data.
      er_entity = request_data.
    ENDIF.

  ENDMETHOD.

  METHOD overviewset_get_entityset.

  data: response_data TYPE zcl_zform_mobile_work_mpc_ext=>ts_overview,
        header_ids type range of uuid.

    DATA(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).

*    select * from zform_headers where employee_number = @current_employee and application = 'MOB_WORK' into table @data(headers).

    data(requests) = zcl_form_request=>from_employee( employee = current_employee application = 'MOB_WORK' ).

    header_ids = value #( for _request in requests ( sign = 'I' option = 'EQ' low = _request->id( ) ) ).

    select * from zform_mob_work where header_id in @header_ids into table @data(form_data).

    loop at requests into data(request).

        data(corresponding_form_data) = form_data[ header_id = request->id( ) ].

        data(mobile_work_hours) = corresponding_form_data-weekly_hours * corresponding_form_data-mobile_work_percent / 100.

        data(approval) = new zcl_form_approver( employee = current_employee approval_step = conv #( request->current_step( ) ) ).

        data(response_entry) = value zcl_zform_mobile_work_mpc_ext=>ts_overview(
        header_id = request->id( )
            begin_date         = corresponding_form_data-begin_date
            next_approver      = approval->get_next_approver( )
            next_approver_name = approval->get_next_approver_name( )
            approval_status    = request->current_step( )
            mobile_work_string = |{ mobile_work_hours }/{ corresponding_form_data-weekly_hours } Stunden ({ corresponding_form_data-mobile_work_percent }%)|
            end_date           = corresponding_form_data-end_date
        ).

        append response_entry to et_entityset.

    endloop.


  ENDMETHOD.

  METHOD dataset_get_entity.
"Zeitraum von 00000101-00000101, heißt neues Formular wird erstellt und es werden Defaultwerte mitgegeben

    data(begin_date) = it_key_tab[ name = 'beginDate' ]-value.
    data(end_date) = it_key_tab[ name = 'endDate' ]-value.
    data(current_employee) = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( ).

if begin_date ne '19700101' and end_date ne '19700101'.


    select single * from zform_mob_work into @data(existing_form) where begin_date = @begin_date and end_date = @end_date and employee_number = @current_employee.


   er_entity = CORRESPONDING #( existing_form ).
*er_entity-substitute = '00000017'.

else.
"default-werte
    er_entity-employee_number = current_employee.
    er_entity-begin_date = sy-datum.
    er_entity-end_date = sy-datum.
    er_entity-mobile_work_percent = 75.
    er_entity-end_date = cl_reca_date=>add_to_date( id_date = er_entity-end_date id_years = 2 ).
    SUBTRACT 1 from er_entity-end_date.
    select single WOSTD from pa0007 where pernr = @current_employee and begda <= @sy-datum and endda >= @sy-datum into @er_entity-weekly_hours.


endif.

me->current_substitute = er_entity-substitute.



  ENDMETHOD.

  METHOD substitutes_get_entityset.

  select distinct pa0000~pernr, pa0002~vorna, pa0002~nachn from pa0000
                                                           JOIN pa0002 on pa0000~pernr = pa0002~pernr and
                                                                          pa0002~begda <= @sy-datum and
                                                                          pa0002~endda >= @sy-datum
                                                           where pa0000~stat2 = '3' into table @et_entityset.


  ENDMETHOD.

  METHOD substitutes_get_entity.
  select single pa0000~pernr, pa0002~vorna, pa0002~nachn from pa0000
                                                           JOIN pa0002 on pa0000~pernr = pa0002~pernr and
                                                                          pa0002~begda <= @sy-datum and
                                                                          pa0002~endda >= @sy-datum
                                                           where pa0000~pernr eq @current_substitute into @er_entity.
  ENDMETHOD.

  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.

    super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entity(
      EXPORTING
        iv_entity_name           = iv_entity_name
        iv_entity_set_name       = iv_entity_set_name
        iv_source_name           = iv_source_name
        it_key_tab               = it_key_tab
        it_navigation_path       = it_navigation_path
        io_expand                = io_expand
        io_tech_request_context  = io_tech_request_context
      IMPORTING
        er_entity                = er_entity
        es_response_context      = es_response_context
        et_expanded_clauses      = et_expanded_clauses
        et_expanded_tech_clauses = et_expanded_tech_clauses
    ).
**    CATCH /iwbep/cx_mgw_busi_exception.
**    CATCH /iwbep/cx_mgw_tech_exception.
  ENDMETHOD.

ENDCLASS.
