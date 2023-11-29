CLASS zcl_form_request DEFINITION
  PUBLIC

  .

  PUBLIC SECTION.

    TYPES: requests TYPE TABLE OF REF TO zcl_form_request WITH EMPTY KEY.


    CLASS-METHODS:
      "! <p class="shorttext synchronized">Erstellt einen neuen Antrag für Formulare</p>
      new IMPORTING employee_number TYPE pernr_d
                    application     TYPE zform_application
          RETURNING VALUE(self)     TYPE REF TO zcl_form_request,
      "! <p class="shorttext synchronized">Selektiert den Antrag anhand der Id</p>
      by_id IMPORTING id          TYPE sysuuid_x16

            RETURNING VALUE(self) TYPE REF TO zcl_form_request,
      "! <p class="shorttext synchronized">Gibt alle Anträge des Mitarbeiters zurück</p>
      from_employee IMPORTING employee TYPE pernr_d
                              application     TYPE zform_application OPTIONAL
                    RETURNING VALUE(requests) TYPE requests.


    METHODS:
      "! <p class="shorttext synchronized">Gibt die Id des Antrags zurück</p>
      id RETURNING VALUE(id) TYPE sysuuid_x16,
      "! <p class="shorttext synchronized">Genehmigt den Antrag</p>
      approve IMPORTING note TYPE string,
      "! <p class="shorttext synchronized">Lehnt den Antrag ab</p>
      reject IMPORTING note TYPE string,
      "! <p class="shorttext synchronized">Gibt den Genehmigungsschritt zurück</p>
      current_step RETURNING VALUE(step) TYPE i,
      "! <p class="shorttext synchronized">Gibt die Notizen zum Antrag zurück</p>
      notes RETURNING VALUE(notes) TYPE zform_note_table.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: latest_header RETURNING VALUE(header) TYPE zform_headers.

    DATA: headers TYPE TABLE OF zform_headers.
ENDCLASS.



CLASS zcl_form_request IMPLEMENTATION.
  METHOD new.

    self = NEW #( ).

    DATA(header_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).

    GET TIME STAMP FIELD DATA(current_timestamp).

    DATA(initial_header) = VALUE zform_headers(
        mandt           = sy-mandt
        id              = header_id
        employee_number = employee_number
        application     = application
        timestamp       = current_timestamp
        approval_step   = 0
        modifier        = sy-uname
    ).


    INSERT zform_headers FROM initial_header.

    IF sy-subrc = 0.
      APPEND initial_header TO self->headers.
    ENDIF.

  ENDMETHOD.

  METHOD id.
    id = headers[ 1 ]-id.
  ENDMETHOD.

  METHOD approve.
    DATA(latest_header) = latest_header( ).

    IF latest_header-approval_step EQ -1 OR
       latest_header-approval_step EQ 3.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(current_timestamp).

    ADD 1 TO latest_header-approval_step.
    latest_header-timestamp = current_timestamp.
    latest_header-modifier = sy-uname.

    DATA(note_entry) = VALUE zform_notes(
        mandt         = sy-mandt
        header_id     = latest_header-id
        approval_step = latest_header-approval_step
        note_from     = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( )
        note          = note
    ).

    INSERT zform_headers FROM latest_header.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    INSERT zform_notes FROM note_entry.


  ENDMETHOD.

  METHOD latest_header.
  sort headers by timestamp ascending.
    header = headers[ lines( headers ) ].
  ENDMETHOD.

  METHOD reject.
    DATA(latest_header) = latest_header( ).

    IF latest_header-approval_step EQ -1.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(current_timestamp).

    latest_header-approval_step = -1.
    latest_header-timestamp = current_timestamp.
    latest_header-modifier = sy-uname.

    DATA(note_entry) = VALUE zform_notes(
        mandt         = sy-mandt
        header_id     = latest_header-id
        approval_step = latest_header-approval_step
        note_from     = cl_hcmfab_employee_api=>get_instance( )->get_employeenumber_from_user( )
        note          = note
    ).

    INSERT zform_headers FROM latest_header.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    INSERT zform_notes FROM note_entry.

  ENDMETHOD.

  METHOD by_id.

    self = NEW #( ).

    SELECT * FROM zform_headers WHERE id EQ @id INTO TABLE @self->headers.

  ENDMETHOD.

  METHOD notes.

    DATA(latest_header) = latest_header( ).

    SELECT * FROM zform_notes WHERE header_id EQ @latest_header-id INTO TABLE @DATA(_notes).

    notes = VALUE #( FOR note IN _notes (
        sender = note-note_from
        message = note-note
        timestamp = headers[ id = note-header_id approval_step = note-approval_step ]-timestamp
        approval_step = note-approval_step
    ) ).


  ENDMETHOD.

  METHOD current_step.
    step = latest_header( )-approval_step.
  ENDMETHOD.

  METHOD from_employee.
    SELECT * FROM zform_headers WHERE employee_number = @employee and application = @application INTO TABLE @DATA(headers).

    SORT headers BY id timestamp.

    LOOP AT headers INTO DATA(header) GROUP BY ( id = header-id ) INTO DATA(group).

      DATA grouped_headers TYPE TABLE OF zform_headers.

      LOOP AT GROUP group INTO header.
        APPEND header TO grouped_headers.
      ENDLOOP.

      DATA(new_request) = NEW zcl_form_request( ).
      new_request->headers = grouped_headers.
      APPEND new_request TO requests.
      CLEAR grouped_headers.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
