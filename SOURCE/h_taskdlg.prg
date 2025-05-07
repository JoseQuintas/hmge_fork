/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 */

#if ! defined( __XHARBOUR__ ) .AND. ( __HARBOUR__ - 0 > 0x030000 )

#include "hbclass.ch"
#include "TaskDlgs.ch"
#include "i_var.ch"

////////////////////////////////////////////////////////////////////////////////
CREATE CLASS TSimpleTaskDialog FUNCTION SimpleTaskDialog
////////////////////////////////////////////////////////////////////////////////
  /*
   *  Class: TSimpleTaskDialog
   *  Purpose:  Provides a simplified interface for displaying Task Dialogs.
   *            This class is designed for basic use cases where only a title, instruction, content,
   *            common buttons, and a main icon are needed.
   */
   EXPORTED:
   /*
    *  Var: Cargo
    *  Type: Variant
    *  Purpose:  A general-purpose variable that can be used to store any data associated with the object.
    *            This is a common practice in HMG to allow attaching custom data to GUI elements.
    */
   VAR    Cargo
   /*
    *  Var: lError
    *  Type: Logical
    *  Purpose:  Indicates whether an error occurred during the execution of the Task Dialog.
    *            It is set to .T. initially and changed to .F. if the dialog executes successfully.
    *  Access: READONLY
    *  Initial Value: .T. (assumes an error until proven otherwise)
    */
   VAR    lError                       READONLY   INIT .T.
   /*
    *  Var: nButtonResult
    *  Type: Numeric
    *  Purpose:  Stores the ID of the button that was clicked by the user in the Task Dialog.
    *            This value is set after the dialog is closed.
    *  Access: READONLY
    *  Initial Value: NIL (no button has been clicked yet)
    */
   VAR    nButtonResult                READONLY   INIT NIL
   /*
    *  Var: nResult
    *  Type: Numeric
    *  Purpose:  Stores the return code of the TaskDialog API function.
    *            This can be used to determine the specific reason for a failure, if any.
    *  Access: READONLY
    *  Initial Value: E_FAIL (indicates a failure until the dialog is executed)
    */
   VAR    nResult                      READONLY   INIT E_FAIL

   /*
    *  Method: New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon )
    *  Purpose:  Constructor for the TSimpleTaskDialog class.  Initializes the dialog's properties.
    *  Parameters:
    *     cTitle:       The title of the Task Dialog (String or Numeric).
    *     cInstruction: The main instruction text (String or Numeric).
    *     cContent:     The content text (String or Numeric).
    *     nCommonButtons: A flag indicating which common buttons to display (Numeric, e.g., TDCBF_OK_BUTTON).
    *     nMainIcon:    A flag indicating which main icon to display (Numeric, e.g., TD_INFORMATION_ICON).
    *  Return Value: Self (the object instance)
    */
   METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon )
   /*
    *  Method: Execute()
    *  Purpose:  Executes the Task Dialog, displaying it to the user.
    *  Parameters: None
    *  Return Value: Logical (.T. if the dialog executed successfully, .F. otherwise)
    */
   METHOD Execute()
   /*
    *  Method: Title( cTitle )
    *  Purpose:  Getter/Setter for the dialog's title.
    *  Parameters:
    *     cTitle: The new title for the dialog (String or Numeric).
    *  Return Value: The old title value.
    */
   METHOD Title( cTitle )              SETGET
   /*
    *  Method: Instruction( cInstruction )
    *  Purpose:  Getter/Setter for the dialog's main instruction text.
    *  Parameters:
    *     cInstruction: The new instruction text (String or Numeric).
    *  Return Value: The old instruction text value.
    */
   METHOD Instruction( cInstruction )  SETGET
   /*
    *  Method: Content( cContent )
    *  Purpose:  Getter/Setter for the dialog's content text.
    *  Parameters:
    *     cContent: The new content text (String or Numeric).
    *  Return Value: The old content text value.
    */
   METHOD Content( cContent )          SETGET
   /*
    *  Method: CommonButtons( nCBs )
    *  Purpose:  Getter/Setter for the common buttons flag.
    *  Parameters:
    *     nCBs: The new common buttons flag (Numeric).
    *  Return Value: The old common buttons flag value.
    */
   METHOD CommonButtons( nCBs )        SETGET
   /*
    *  Method: MainIcon( nIcon )
    *  Purpose:  Getter/Setter for the main icon flag.
    *  Parameters:
    *     nIcon: The new main icon flag (Numeric).
    *  Return Value: The old main icon flag value.
    */
   METHOD MainIcon( nIcon )            SETGET

   PROTECTED:
   /*
    *  Var: cTitle
    *  Type: String
    *  Purpose:  Stores the title of the Task Dialog.
    *  Initial Value: NIL
    */
   VAR    cTitle                       INIT       NIL
   /*
    *  Var: cInstruction
    *  Type: String
    *  Purpose:  Stores the main instruction text of the Task Dialog.
    *  Initial Value: NIL
    */
   VAR    cInstruction                 INIT       NIL
   /*
    *  Var: cContent
    *  Type: String
    *  Purpose:  Stores the content text of the Task Dialog.
    *  Initial Value: NIL
    */
   VAR    cContent                     INIT       NIL
   /*
    *  Var: nCommonButtons
    *  Type: Numeric
    *  Purpose:  Stores the flag indicating which common buttons to display.
    *  Initial Value: TDCBF_OK_BUTTON (displays only the OK button by default)
    */
   VAR    nCommonButtons               INIT       TDCBF_OK_BUTTON
   /*
    *  Var: nMainIcon
    *  Type: Numeric
    *  Purpose:  Stores the flag indicating which main icon to display.
    *  Initial Value: TD_NO_ICON (displays no icon by default)
    */
   VAR    nMainIcon                    INIT       TD_NO_ICON

ENDCLASS
////////////////////////////////////////////////////////////////////////////////

/*
 *  Method: New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog
 *  Purpose:  Constructor for the TSimpleTaskDialog class.  Initializes the dialog's properties.
 *  Parameters:
 *     cTitle:       The title of the Task Dialog (String or Numeric).
 *     cInstruction: The main instruction text (String or Numeric).
 *     cContent:     The content text (String or Numeric).
 *     nCommonButtons: A flag indicating which common buttons to display (Numeric, e.g., TDCBF_OK_BUTTON).
 *     nMainIcon:    A flag indicating which main icon to display (Numeric, e.g., TD_INFORMATION_ICON).
 *  Return Value: Self (the object instance)
 */
METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog

   // Assign the title, instruction, and content, handling different data types and NULL values.
   ::cTitle       := iif( HB_ISNUMERIC( cTitle ), cTitle, iif( ! HB_ISSTRING( cTitle ), NIL, iif( HB_ISNULL( cTitle ), NIL, cTitle ) ) )
   ::cInstruction := iif( HB_ISNUMERIC( cInstruction ), cInstruction, iif( ! HB_ISSTRING( cInstruction ), NIL, iif( HB_ISNULL( cInstruction ), NIL, cInstruction ) ) )
   ::cContent     := iif( HB_ISNUMERIC( cContent ), cContent, iif( ! HB_ISSTRING( cContent ), NIL, iif( HB_ISNULL( cContent ), NIL, cContent ) ) )

   // If a numeric value is provided for common buttons, assign it.
   IF HB_ISNUMERIC( nCommonButtons )
      ::nCommonButtons := nCommonButtons
   ENDIF

   // If a numeric value is provided for the main icon, assign it.
   IF HB_ISNUMERIC( nMainIcon )
      ::nMainIcon := nMainIcon
   ENDIF

RETURN Self

/*
 *  Method: Execute() CLASS TSimpleTaskDialog
 *  Purpose:  Executes the Task Dialog, displaying it to the user.
 *  Parameters: None
 *  Return Value: Logical (.T. if the dialog executed successfully, .F. otherwise)
 */
METHOD Execute() CLASS TSimpleTaskDialog

   LOCAL nResult
   LOCAL nButton := NIL

   // Initialize error flag and result variables.
   ::lError        := .T.
   ::nButtonResult := NIL
   ::nResult       := E_FAIL

   // Check if the operating system is Windows Vista or later.
   IF os_IsWinVista_Or_Later()
      // Call the win_TaskDialog0 function to display the Task Dialog.
      // The @nButton passes the variable by reference so the function can set the value.
      nResult := win_TaskDialog0( ,, ::cTitle, ::cInstruction, ::cContent, ::nCommonButtons, ::nMainIcon, @nButton )
   ELSE
      // If the operating system is older than Windows Vista, set the result to E_NOTIMPL (Not Implemented).
      nResult := E_NOTIMPL // Not implemented yet
   ENDIF

   // Update the object's properties based on the result of the Task Dialog execution.
   ::lError        := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nResult       := nResult

RETURN ( ! ::lError )

/*
 *  Method: Title( cTitle ) CLASS TSimpleTaskDialog
 *  Purpose:  Getter/Setter for the dialog's title.
 *  Parameters:
 *     cTitle: The new title for the dialog (String or Numeric).
 *  Return Value: The old title value.
 */
METHOD Title( cTitle ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cTitle

   // If the new title is a string or numeric value, assign it.
   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      // If the new title is a NULL string, set the title to NIL.
      ::cTitle := iif( HB_ISSTRING( cTitle ) .AND. HB_ISNULL( cTitle ), NIL, cTitle )
   ENDIF

RETURN cOldVal

/*
 *  Method: Instruction( cInstruction ) CLASS TSimpleTaskDialog
 *  Purpose:  Getter/Setter for the dialog's main instruction text.
 *  Parameters:
 *     cInstruction: The new instruction text (String or Numeric).
 *  Return Value: The old instruction text value.
 */
METHOD Instruction( cInstruction ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cInstruction

   // If the new instruction is a string or numeric value, assign it.
   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      // If the new instruction is a NULL string, set the instruction to NIL.
      ::cInstruction := iif( HB_ISSTRING( cInstruction ) .AND. HB_ISNULL( cInstruction ), NIL, cInstruction )
   ENDIF

RETURN cOldVal

/*
 *  Method: Content( cContent ) CLASS TSimpleTaskDialog
 *  Purpose:  Getter/Setter for the dialog's content text.
 *  Parameters:
 *     cContent: The new content text (String or Numeric).
 *  Return Value: The old content text value.
 */
METHOD Content( cContent ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cContent

   // If the new content is a string or numeric value, assign it.
   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      // If the new content is a NULL string, set the content to NIL.
      ::cContent := iif( HB_ISSTRING( cContent ) .AND. HB_ISNULL( cContent ), NIL, cContent )
   ENDIF

RETURN cOldVal

/*
 *  Method: CommonButtons( nCBs ) CLASS TSimpleTaskDialog
 *  Purpose:  Getter/Setter for the common buttons flag.
 *  Parameters:
 *     nCBs: The new common buttons flag (Numeric).
 *  Return Value: The old common buttons flag value.
 */
METHOD CommonButtons( nCBs ) CLASS TSimpleTaskDialog

   LOCAL nOldVal := ::nCommonButtons

   // If the new common buttons flag is a numeric value, assign it.
   IF HB_ISNUMERIC( nCBs )
      ::nCommonButtons := nCBs
   ENDIF

RETURN nOldVal

/*
 *  Method: MainIcon( nIcon ) CLASS TSimpleTaskDialog
 *  Purpose:  Getter/Setter for the main icon flag.
 *  Parameters:
 *     nIcon: The new main icon flag (Numeric).
 *  Return Value: The old main icon flag value.
 */
METHOD MainIcon( nIcon ) CLASS TSimpleTaskDialog

   LOCAL nOldVal := ::nMainIcon

   // If the new main icon flag is a numeric value, assign it.
   IF HB_ISNUMERIC( nIcon )
      ::nMainIcon := nIcon
   ENDIF

RETURN nOldVal

////////////////////////////////////////////////////////////////////////////////
CREATE CLASS TTaskDialog FUNCTION TaskDialog
////////////////////////////////////////////////////////////////////////////////
  /*
   *  Class: TTaskDialog
   *  Purpose:  Provides a comprehensive interface for displaying Task Dialogs with advanced features.
   *            This class allows for customization of various aspects of the dialog, including custom buttons,
   *            radio buttons, verification checkboxes, and expanded information.
   */
   EXPORTED:
   /*
    *  Var: Cargo
    *  Type: Variant
    *  Purpose:  A general-purpose variable that can be used to store any data associated with the object.
    *            This is a common practice in HMG to allow attaching custom data to GUI elements.
    */
   VAR    Cargo
   /*
    *  Var: lActive
    *  Type: Logical
    *  Purpose:  Indicates whether the Task Dialog is currently active (being displayed).
    *  Access: READONLY
    *  Initial Value: .F. (the dialog is not active initially)
    */
   VAR    lActive               READONLY   INIT .F.
   /*
    *  Var: lError
    *  Type: Logical
    *  Purpose:  Indicates whether an error occurred during the execution of the Task Dialog.
    *            It is set to .T. initially and changed to .F. if the dialog executes successfully.
    *  Access: READONLY
    *  Initial Value: .T. (assumes an error until proven otherwise)
    */
   VAR    lError                READONLY   INIT .T.
   /*
    *  Var: nButtonResult
    *  Type: Numeric
    *  Purpose:  Stores the ID of the button that was clicked by the user in the Task Dialog.
    *            This value is set after the dialog is closed.
    *  Access: READONLY
    *  Initial Value: NIL (no button has been clicked yet)
    */
   VAR    nButtonResult         READONLY   INIT NIL
   /*
    *  Var: nRadioButtonResult
    *  Type: Numeric
    *  Purpose:  Stores the ID of the radio button that was selected by the user in the Task Dialog.
    *            This value is set after the dialog is closed.
    *  Access: READONLY
    *  Initial Value: NIL (no radio button has been selected yet)
    */
   VAR    nRadioButtonResult    READONLY   INIT NIL
   /*
    *  Var: nResult
    *  Type: Numeric
    *  Purpose:  Stores the return code of the TaskDialog API function.
    *            This can be used to determine the specific reason for a failure, if any.
    *  Access: READONLY
    *  Initial Value: E_FAIL (indicates a failure until the dialog is executed)
    */
   VAR    nResult               READONLY   INIT E_FAIL
   /*
    *  Var: lVerifyResult
    *  Type: Logical
    *  Purpose:  Stores the state of the verification checkbox in the Task Dialog.
    *            This value is set after the dialog is closed.
    *  Access: READONLY
    *  Initial Value: .F. (the checkbox is not checked initially)
    */
   VAR    lVerifyResult         READONLY   INIT .F.

   /*
    *  Method: New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon )
    *  Purpose:  Constructor for the TTaskDialog class.  Initializes the dialog's properties.
    *  Parameters:
    *     cTitle:       The title of the Task Dialog (String or Numeric).
    *     cInstruction: The main instruction text (String or Numeric).
    *     cContent:     The content text (String or Numeric).
    *     cFooter:      The footer text (String or Numeric).
    *     nCommonButtons: A flag indicating which common buttons to display (Numeric, e.g., TDCBF_OK_BUTTON).
    *     nMainIcon:    A flag indicating which main icon to display (Numeric, e.g., TD_INFORMATION_ICON).
    *  Return Value: Self (the object instance)
    */
   METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon )
   /*
    *  Method: Execute()
    *  Purpose:  Executes the Task Dialog, displaying it to the user.  This is an INLINE method that calls ShowDialog().
    *  Parameters: None
    *  Return Value: None (calls ShowDialog() which returns a logical value indicating success)
    */
   METHOD Execute() INLINE ::ShowDialog()
   /*
    *  Method: ShowDialog()
    *  Purpose:  Displays the Task Dialog to the user.
    *  Parameters: None
    *  Return Value: Logical (.T. if the dialog executed successfully, .F. otherwise)
    */
   METHOD ShowDialog()
   /*
    *  Method: DialogHandle()
    *  Purpose:  Returns the handle (HWND) of the Task Dialog window.
    *  Parameters: None
    *  Return Value: The window handle (HWND) of the Task Dialog.  Returns NIL if the dialog is not active.
    */
   METHOD DialogHandle()
   /*
    *  Method: Showing( lState )
    *  Purpose:  Getter/Setter for the dialog's active state (whether it is currently being displayed).
    *  Parameters:
    *     lState:  A logical value indicating whether to show (.T.) or hide (.F.) the dialog.
    *  Return Value: The current active state of the dialog (Logical).
    */
   METHOD Showing( lState )
   /*
    *  Method: OnCreated( hWnd, nNotify, nWParam, nLParam )
    *  Purpose:  Event handler called when the Task Dialog is created.
    *  Parameters:
    *     hWnd:    The handle of the Task Dialog window.
    *     nNotify: The notification code (TDN_CREATED).
    *     nWParam: Additional information (unused).
    *     nLParam: Additional information (unused).
    *  Return Value: Logical (.F. to allow default processing)
    */
   METHOD OnCreated( hWnd, nNotify, nWParam, nLParam )
   /*
    *  Method: OnDestroyed( hWnd, nNotify, nWParam, nLParam )
    *  Purpose:  Event handler called when the Task Dialog is destroyed.
    *  Parameters:
    *     hWnd:    The handle of the Task Dialog window.
    *     nNotify: The notification code (TDN_DESTROYED).
    *     nWParam: Additional information (unused).
    *     nLParam: Additional information (unused).
    *  Return Value: Logical (.F. to allow default processing)
    */
   METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam )
   /*
    *  Method: Listener( hWnd, nNotify, nWParam, nLParam )
    *  Purpose:  The default event listener for the Task Dialog.  Calls the callback block if one is defined.
    *  Parameters:
    *     hWnd:    The handle of the Task Dialog window.
    *     nNotify: The notification code.
    *     nWParam: Additional information.
    *     nLParam: Additional information.
    *  Return Value: Logical (.T. to allow default processing, .F. to prevent it)
    */
   METHOD Listener( hWnd, nNotify, nWParam, nLParam )
   /*
    *  Method: CommonButtons( nCBs )
    *  Purpose:  Getter/Setter for the common buttons flag.
    *  Parameters:
    *     nCBs: The new common buttons flag (Numeric).
    *  Return Value: The old common buttons flag value.
    */
   METHOD CommonButtons( nCBs )                SETGET
   /*
    *  Method: WindowTitle( cTitle )
    *  Purpose:  Getter/Setter for the dialog's window title.
    *  Parameters:
    *     cTitle: The new window title (String or Numeric).
    *  Return Value: The old window title value.
    */
   METHOD WindowTitle( cTitle )                SETGET
   /*
    *  Method: Title( cTitle )
    *  Purpose:  Getter/Setter for the dialog's title (same as WindowTitle).
    *  Parameters:
    *     cTitle: The new title (String or Numeric).
    *  Return Value: The old title value.
    */
   METHOD Title( cTitle )                      SETGET
   /*
    *  Method: MainIcon( nIcon )
    *  Purpose:  Getter/Setter for the main icon flag.
    *  Parameters:
    *     nIcon: The new main icon flag (Numeric).
    *  Return Value: The old main icon flag value.
    */
   METHOD MainIcon( nIcon )                    SETGET
   /*
    *  Method: MainInstruction( cInstruction )
    *  Purpose:  Getter/Setter for the dialog's main instruction text.
    *  Parameters:
    *     cInstruction: The new instruction text (String or Numeric).
    *  Return Value: The old instruction text value.
    */
   METHOD MainInstruction( cInstruction )      SETGET
   /*
    *  Method: Instruction( cInstruction )
    *  Purpose:  Getter/Setter for the dialog's main instruction text (same as MainInstruction).
    *  Parameters:
    *     cInstruction: The new instruction text (String or Numeric).
    *  Return Value: The old instruction text value.
    */
   METHOD Instruction( cInstruction )          SETGET
   /*
    *  Method: Content( cContent )
    *  Purpose:  Getter/Setter for the dialog's content text.
    *  Parameters:
    *     cContent: The new content text (String or Numeric).
    *  Return Value: The old content text value.
    */
   METHOD Content( cContent )                  SETGET
   /*
    *  Method: CustomButtons( aCustButton )
    *  Purpose:  Getter/Setter for the array of custom buttons.
    *  Parameters:
    *     aCustButton: An array of custom button definitions.
    *  Return Value: The old array of custom button definitions.
    */
   METHOD CustomButtons( aCustButton )         SETGET
   /*
    *  Method: DefaultButton( nDefaultButton )
    *  Purpose:  Getter/Setter for the ID of the default button.
    *  Parameters:
    *     nDefaultButton: The ID of the default button (Numeric).
    *  Return Value: The old ID of the default button.
    */
   METHOD DefaultButton( nDefaultButton )      SETGET
   /*
    *  Method: CustomRadioButtons( aCustButton )
    *  Purpose:  Getter/Setter for the array of custom radio buttons.
    *  Parameters:
    *     aCustButton: An array of custom radio button definitions.
    *  Return Value: The old array of custom radio button definitions.
    */
   METHOD CustomRadioButtons( aCustButton )    SETGET
   /*
    *  Method: DefaultRadioButton( nDefaultButton )
    *  Purpose:  Getter/Setter for the ID of the default radio button.
    *  Parameters:
    *     nDefaultButton: The ID of the default radio button (Numeric).
    *  Return Value: The old ID of the default radio button.
    */
   METHOD DefaultRadioButton( nDefaultButton ) SETGET
   /*
    *  Method: VerificationText( cText )
    *  Purpose:  Getter/Setter for the text of the verification checkbox.
    *  Parameters:
    *     cText: The new text for the verification checkbox (String or Numeric).
    *  Return Value: The old text of the verification checkbox.
    */
   METHOD VerificationText( cText )            SETGET
   /*
    *  Method: ExpandedInfo( cText )
    *  Purpose:  Getter/Setter for the expanded information text.
    *  Parameters:
    *     cText: The new expanded information text (String or Numeric).
    *  Return Value: The old expanded information text.
    */
   METHOD ExpandedInfo( cText )                SETGET
   /*
    *  Method: ExpandedControlText( cText )
    *  Purpose:  Getter/Setter for the text of the button that collapses the expanded information.
    *  Parameters:
    *     cText: The new text for the collapse button (String or Numeric).
    *  Return Value: The old text of the collapse button.
    */
   METHOD ExpandedControlText( cText )         SETGET
   /*
    *  Method: ExpandedCtrlText( cText )
    *  Purpose:  Alias for ExpandedControlText.
    *  Parameters:
    *     cText: The new text for the collapse button (String or Numeric).
    *  Return Value: The old text of the collapse button.
    */
   METHOD ExpandedCtrlText( cText )            SETGET
   /*
    *  Method: CollapsedControlText( cText )
    *  Purpose:  Getter/Setter for the text of the button that expands the expanded information.
    *  Parameters:
    *     cText: The new text for the expand button (String or Numeric).
    *  Return Value: The old text of the expand button.
    */
   METHOD CollapsedControlText( cText )        SETGET
   /*
    *  Method: CollapsedCtrlText( cText )
    *  Purpose:  Alias for CollapsedControlText.
    *  Parameters:
    *     cText: The new text for the expand button (String or Numeric).
    *  Return Value: The old text of the expand button.
    */
   METHOD CollapsedCtrlText( cText )           SETGET
   /*
    *  Method: FooterIcon( nIcon )
    *  Purpose:  Getter/Setter for the footer icon flag.
    *  Parameters:
    *     nIcon: The new footer icon flag (Numeric).
    *  Return Value: The old footer icon flag value.
    */
   METHOD FooterIcon( nIcon )                  SETGET
   /*
    *  Method: Footer( cFooter )
    *  Purpose:  Getter/Setter for the dialog's footer text.
    *  Parameters:
    *     cFooter: The new footer text (String or Numeric).
    *  Return Value: The old footer text.
    */
   METHOD Footer( cFooter )                    SETGET
   /*
    *  Method: Width( nWidth )
    *  Purpose:  Getter/Setter for the width of the dialog's client area.
    *  Parameters:
    *     nWidth: The new width in dialog units (Numeric).
    *  Return Value: The old width.
    */
   METHOD Width( nWidth )                      SETGET
   /*
    *  Method: Parent( cFormName )
    *  Purpose:  Getter/Setter for the name of the parent form.
    *  Parameters:
    *     cFormName: The name of the parent form (String).
    *  Return Value: The old name of the parent form.
    */
   METHOD Parent( cFormName )                  SETGET
   /*
    *  Method: ParentHandle( nHandle )
    *  Purpose:  Getter/Setter for the handle of the parent window.
    *  Parameters:
    *     nHandle: The handle of the parent window (Numeric).
    *  Return Value: The old handle of the parent window.
    */
   METHOD ParentHandle( nHandle )              SETGET
   /*
    *  Method: CallBackBlock( bCode )
    *  Purpose:  Getter/Setter for the callback block that is executed for Task Dialog events.
    *  Parameters:
    *     bCode: The callback block (Eval Item).
    *  Return Value: The old callback block.
    */
   METHOD CallBackBlock( bCode )               SETGET
   /*
    *  Method: Flags( nFlags )
    *  Purpose:  Getter/Setter for the Task Dialog flags.
    *  Parameters:
    *     nFlags: The new Task Dialog flags (Numeric).
    *  Return Value: The old Task Dialog flags.
    */
   METHOD Flags( nFlags )                      SETGET
   /*
    *  Method: AllowDialogCancellation( lNewVal )
    *  Purpose:  Getter/Setter for the flag that allows the dialog to be cancelled.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to allow cancellation (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD AllowDialogCancellation( lNewVal )   SETGET
   /*
    *  Method: CanBeMinimized( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether the dialog can be minimized.
    *  Parameters:
    *     lNewVal: A logical value indicating whether the dialog can be minimized (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD CanBeMinimized( lNewVal )            SETGET
   /*
    *  Method: EnableHyperlinks( lNewVal )
    *  Purpose:  Getter/Setter for the flag that enables hyperlinks in the content, expanded information, and footer.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to enable hyperlinks (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD EnableHyperlinks( lNewVal )          SETGET
   /*
    *  Method: ExpandedByDefault( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether the expanded information is displayed by default.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to expand by default (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD ExpandedByDefault( lNewVal )         SETGET
   /*
    *  Method: ExpandFooterArea( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether the expanded information is displayed in the footer area.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to expand in the footer area (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD ExpandFooterArea( lNewVal )          SETGET
   /*
    *  Method: NoDefaultRadioButton( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether no default radio button should be selected.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to select no default radio button (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD NoDefaultRadioButton( lNewVal )      SETGET
   /*
    *  Method: PositionRelativeToWindow( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether the dialog is positioned relative to the parent window.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to position relative to the window (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD PositionRelativeToWindow( lNewVal )  SETGET
   /*
    *  Method: RightToLeftLayout( lNewVal )
    *  Purpose:  Getter/Setter for the flag that indicates whether the text is displayed right to left.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to use right-to-left layout (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD RightToLeftLayout( lNewVal )         SETGET
   /*
    *  Method: VerificationEnabled( lNewVal )
    *  Purpose:  Getter/Setter for the flag that enables the verification checkbox.
    *  Parameters:
    *     lNewVal: A logical value indicating whether to enable the verification checkbox (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD VerificationEnabled( lNewVal )       SETGET
   /*
    *  Method: timeoutMS( nMS )
    *  Purpose:  Getter/Setter for the timeout value in milliseconds.
    *  Parameters:
    *     nMS: The new timeout value in milliseconds (Numeric).
    *  Return Value: The old timeout value.
    */
   METHOD timeoutMS( nMS )                     SETGET
   /*
    *  Method: TimedOut( lOut )
    *  Purpose:  Getter/Setter for the flag that indicates whether the dialog timed out.
    *  Parameters:
    *     lOut: A logical value indicating whether the dialog timed out (.T.) or not (.F.).
    *  Return Value: The old value of the flag.
    */
   METHOD TimedOut( lOut )                     SETGET
   // NOTE: Next method returns valid (non NIL) result if a the dialog has been shown
   // The ID of the clicked button
   METHOD SelectedButton()      INLINE ::nButtonResult  // Returns the ID of the button that was clicked. INLINE:  The compiler may choose to replace the call to SelectedButton() with the code of ::nButtonResult directly for performance.
   // The ID of the selected radio button
   METHOD SelectedRadioButton() INLINE ::nRadioButtonResult  // Returns the ID of the radio button that was selected. INLINE:  The compiler may choose to replace the call to SelectedRadioButton() with the code of ::nRadioButtonResult directly for performance.
   // The state of the verification checkbox (read only)
   METHOD VerificationChecked() INLINE ::lVerifyResult  // Returns the state of the verification checkbox. INLINE:  The compiler may choose to replace the call to VerificationChecked() with the code of ::lVerifyResult directly for performance.

   PROTECTED:
   VAR aConfig                  INIT Array( TDC_CONFIG )  // Array to store the configuration parameters for the TaskDialog.  TDC_CONFIG is a constant representing the number of configuration parameters. INIT Array( TDC_CONFIG ): Initializes the array with a specific size.
   VAR HWND            READONLY INIT NIL  // Stores the Windows handle (HWND) of the TaskDialog. READONLY: Can only be set internally by the class. INIT NIL: Initialized to NIL (no handle yet).
   VAR lTimeOut        READONLY INIT .F.  // Flag indicating whether the TaskDialog timed out. READONLY: Can only be set internally by the class. INIT .F.: Initialized to False (no timeout yet).
   VAR nTimeOutMS      READONLY INIT 0  // Stores the timeout value in milliseconds. READONLY: Can only be set internally by the class. INIT 0: Initialized to 0 (no timeout).

ENDCLASS
////////////////////////////////////////////////////////////////////////////////

METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon ) CLASS TTaskDialog

   ::aConfig[ TDC_WINDOWTITLE ]     := iif( HB_ISNUMERIC( cTitle ), cTitle, iif( ! HB_ISSTRING( cTitle ), NIL, iif( HB_ISNULL( cTitle ), NIL, cTitle ) ) )
   ::aConfig[ TDC_MAININSTRUCTION ] := iif( HB_ISNUMERIC( cInstruction ), cInstruction, iif( ! HB_ISSTRING( cInstruction ), NIL, iif( HB_ISNULL( cInstruction ), NIL, cInstruction ) ) )
   ::aConfig[ TDC_CONTENT ] := iif( HB_ISNUMERIC( cContent ), cContent, iif( ! HB_ISSTRING( cContent ), NIL, iif( HB_ISNULL( cContent ), NIL, cContent ) ) )
   ::aConfig[ TDC_FOOTER ]  := iif( HB_ISNUMERIC( cFooter ), cFooter, iif( ! HB_ISSTRING( cFooter ), NIL, iif( HB_ISNULL( cFooter ), NIL, cFooter ) ) )

   IF HB_ISNUMERIC( nCommonButtons )
      ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCommonButtons
   ENDIF

   IF HB_ISNUMERIC( nMainIcon )
      ::aConfig[ TDC_MAINICON ] := nMainIcon
   ENDIF

RETURN Self

/*
   Shows the dialog.

   NOTE: Returns true if everything worked right. Returns false if creation of dialog failed.
   Requires Windows Vista or newer.
 */
METHOD ShowDialog() CLASS TTaskDialog

   LOCAL nResult
   LOCAL nButton      := NIL
   LOCAL nRadioButton := NIL
   LOCAL lVerificationFlagChecked := .F.

   IF ! ::lActive
      ::lError             := .T.
      ::nButtonResult      := NIL
      ::nRadioButtonResult := NIL
      ::nResult            := E_FAIL
      ::TimedOut           := .F.

      IF ::timeoutMS() > 0 .OR. __objHasMethod( Self, "ONTIMER" )
         ::Flags := hb_bitOr( ::Flags, TDF_CALLBACK_TIMER )
      ENDIF

      IF ::timeoutMS() > 0
         ::AllowDialogCancellation := .T.
      ENDIF

      IF os_IsWinVista_Or_Later()
         ::aConfig[ 23 ] := self
         nResult := win_TaskDialogIndirect0( ::aConfig, @nButton, @nRadioButton, @lVerificationFlagChecked )
      ELSE
         nResult := E_NOTIMPL // Not implemented yet
      ENDIF

      ::lError             := !( nResult == NOERROR )
      ::nButtonResult      := nButton
      ::nRadioButtonResult := nRadioButton
      ::lVerifyResult      := lVerificationFlagChecked
      ::nResult            := nResult
   ENDIF

RETURN ( ! ::lError )

/*
   The handle of the dialog.

   NOTE: This is only valid (and non NIL) while dialog is visible (read only).
 */
METHOD DialogHandle() CLASS TTaskDialog
RETURN ::HWND

/*
   Whether dialog is currently showing (read/write).
 */
METHOD Showing( lState ) CLASS TTaskDialog

   hb_default( @lState, .F. )

   IF lState .AND. ! ::lActive
      ::ShowDialog()
   ENDIF

RETURN ::lActive

/*
   Indicates that the Task Dialog has been created.
*/
METHOD OnCreated( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )

   IF nNotify == TDN_CREATED
      ::lActive := .T.
      ::HWND := hWnd
   ENDIF

RETURN .F.

/*
   Indicates that the Task Dialog has been destroyed.
*/
METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )

   IF nNotify == TDN_DESTROYED
      ::lActive := .F.
      ::HWND := Nil
   ENDIF

RETURN .F.

/*
   The default Events Listener.
*/
METHOD Listener( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( hWnd )

   IF HB_ISEVALITEM( ::aConfig[ TDC_CALLBACK ] )
      RETURN ::aConfig[ TDC_CALLBACK ]:Eval( self, nNotify, nWParam, nLParam )
   ENDIF

RETURN .T.

/*
   Specifies the push buttons displayed in the task dialog (read/write).

   NOTE:  If  no  common  buttons  are  specified  and  no  custom buttons are
   specified through buttons array, the task dialog will contain the OK button
   by default.
 */
METHOD CommonButtons( nCBs ) CLASS TTaskDialog

   LOCAL nOldCBS := ::aConfig[ TDC_COMMON_BUTTON_FLAGS ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nCBs )
         ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCBs
      ENDIF
   ENDIF

RETURN nOldCBS

/*
   The string to be used for the task dialog title (read/write, LIVE).
 */
METHOD WindowTitle( cTitle ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_WINDOWTITLE ]

   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      ::aConfig[ TDC_WINDOWTITLE ] := iif( HB_ISSTRING( cTitle ) .AND. HB_ISNULL( cTitle ), NIL, cTitle )
      IF ::lActive
         _SetWindowTitle( ::HWND, ::aConfig[ TDC_WINDOWTITLE ] )
      ENDIF
   ENDIF

RETURN cOldVal

METHOD Title( cTitle ) CLASS TTaskDialog
RETURN ::WindowTitle( cTitle )

/*
   TODO
*/
METHOD MainIcon( nIcon ) CLASS TTaskDialog

   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_MAINICON ] := nIcon
      IF ::lActive
         _UpdateMainIcon( ::HWND, ::aConfig[ TDC_MAINICON ] )
      ENDIF
   ENDIF

RETURN ::aConfig[ TDC_MAINICON ]

/*
   The string to be used for the main instruction (read/write, LIVE).
 */
METHOD MainInstruction( cInstruction ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_MAININSTRUCTION ]

   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      ::aConfig[ TDC_MAININSTRUCTION ] := iif( HB_ISSTRING( cInstruction ) .AND. HB_ISNULL( cInstruction ), NIL, cInstruction )
      IF ::lActive
         _SetMainInstruction( ::HWND, ::aConfig[ TDC_MAININSTRUCTION ] )
      ENDIF
   ENDIF

RETURN cOldVal

METHOD Instruction( cInstruction ) CLASS TTaskDialog
RETURN ::MainInstruction( cInstruction )

/*
   The string to be used for the dialog's primary content (read/write, LIVE).
 */
METHOD Content( cContent ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_CONTENT ]

   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      ::aConfig[ TDC_CONTENT ] := iif( HB_ISSTRING( cContent ) .AND. HB_ISNULL( cContent ), NIL, cContent )
      IF ::lActive
         _SetContent( ::HWND, ::aConfig[ TDC_CONTENT ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*
   TODO
*/
METHOD CustomButtons( aCustButton ) CLASS TTaskDialog

   LOCAL aOldVal := ::aConfig[ TDC_TASKDIALOG_BUTTON ]

   IF ! ::lActive
      IF HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
         ::aConfig[ TDC_BUTTON ] := Len( aCustButton )
         ::aConfig[ TDC_TASKDIALOG_BUTTON ] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*
   The default button for the task dialog (read/write).

   Note:  This may be any of the values specified in ID of one of the buttons,
   or   one  of  the  IDs  corresponding  to  the  buttons  specified  in  the
   CommonButtons property.
*/
METHOD DefaultButton( nDefaultButton ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTBUTTON ] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*
   TODO
*/
METHOD CustomRadioButtons( aCustButton ) CLASS TTaskDialog

   LOCAL aOldVal := ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ]

   IF ! ::lActive
      IF HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
         ::aConfig[ TDC_RADIOBUTTON ] := Len( aCustButton )
         ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*
   The button ID of the radio button that is selected by default (read/write).

   NOTE: If this value does not correspond to a button ID, the first button in the array is selected by default.
 */
METHOD DefaultRadioButton( nDefaultButton ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTRADIOBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTRADIOBUTTON ] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*
   The string to be used to label the verification checkbox (read/write).
*/
METHOD VerificationText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_VERIFICATIONTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_VERIFICATIONTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

/* ExpandedInformation
   The string to be used for displaying additional information (read/write,
   LIVE).

   NOTE:  The additional information is displayed either immediately below the
   content  or below the footer text depending on whether the ExpandFooterArea
   flag  is  true.  If the EnableHyperlinks flag is true, then this string may
   contain   hyperlinks  in  the  form:

   <A  HREF="executablestring">Hyperlink Text</A>.

   WARNING:  Enabling  hyperlinks when using content from an unsafe source may
   cause security vulnerabilities.
 */
METHOD ExpandedInfo( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_EXPANDEDINFORMATION ]

   IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
      ::aConfig[ TDC_EXPANDEDINFORMATION ] := cText
      IF ::lActive
         _SetExpandedInformation( ::HWND, ::aConfig[ TDC_EXPANDEDINFORMATION ] )
      ENDIF
   ENDIF

RETURN cOldVal

/* ExpandedControlText
   The  string  to  be  used to label the button for collapsing the expandable
   information (read/write).

   NOTE: This member is ignored when the ExpandedInformation member is empty.
   If this member is empty and the CollapsedControlText is specified, then the
   CollapsedControlText value will be used for this member as well.
 */
METHOD ExpandedControlText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_EXPANDEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_EXPANDEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

METHOD ExpandedCtrlText( cText ) CLASS TTaskDialog
RETURN ::ExpandedControlText( cText )

/* CollapsedControlText
   The  string  to  be  used  to label the button for expanding the expandable
   information (read/write).

   NOTE: This member  is ignored when the ExpandedInformation member is empty.
   If this member is empty and the CollapsedControlText is specified, then the
   CollapsedControlText value will be used for this member as well.
 */
METHOD CollapsedControlText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

METHOD CollapsedCtrlText( cText ) CLASS TTaskDialog
RETURN ::CollapsedControlText( cText )

/*
   TODO
*/
METHOD FooterIcon( nIcon ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_FOOTERICON ]

   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_FOOTERICON ] := nIcon
      IF ::lActive
         _UpdateFooterIcon( ::HWND, ::aConfig[ TDC_FOOTERICON ] )
      ENDIF
   ENDIF

RETURN nOldVal

/*
   The string to be used in the footer area of the task dialog (read/write).

   NOTE: If EnableHyperlinks is true, this can show clickable links.
 */
METHOD Footer( cFooter ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_FOOTER ]

   IF HB_ISSTRING( cFooter ) .OR. HB_ISNUMERIC( cFooter )
      ::aConfig[ TDC_FOOTER ] := cFooter
      IF ::lActive
         _SetFooter( ::HWND, ::aConfig[ TDC_FOOTER ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*
   The width of the task dialog's client area, in dialog units (read/write).

   NOTE: If 0, the task dialog manager will calculate the ideal width.
 */
METHOD Width( nWidth ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_WIDTH ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nWidth )
      ::aConfig[ TDC_WIDTH ] := nWidth
   ENDIF

RETURN nOldVal

/*
   Parent window handle (read/write).
 */
METHOD ParentHandle( nHandle ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_HWND ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nHandle ) .AND. IsWindowHandle( nHandle )
      ::aConfig[ TDC_HWND ] := nHandle
   ENDIF

RETURN nOldVal

/*
   Parent window name (read/write).
 */
METHOD Parent( cFormName ) CLASS TTaskDialog
RETURN _HMG_aFormNames[ AScan ( _HMG_aFormHandles, ::ParentHandle( GetFormHandle( cFormName ) ) ) ]

/*
   NOTE: Method CallBackBlock will be deleted in future (not near)
*/
METHOD CallBackBlock( bCode ) CLASS TTaskDialog

   IF ! ::lActive
      IF HB_ISEVALITEM( bCode )
         ::aConfig[ TDC_CALLBACK ] := bCode
      ENDIF
   ENDIF

RETURN ::aConfig[ TDC_CALLBACK ]

/*
   The flags (read/write).

   NOTE:  Maybe You should not need to set flags as we have properties for all
   relevant flags.
 */
METHOD Flags( nFlags ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_TASKDIALOG_FLAGS ]
   IF ! ::lActive
      IF HB_ISNUMERIC( nFlags )
         ::aConfig[ TDC_TASKDIALOG_FLAGS ] := nFlags
      ENDIF
   ENDIF

RETURN nOldVal

/*
   Whether to allow cancel (read/write).

   NOTE: Indicates that the dialog  should be  able to be closed using Alt-F4,
   Escape,  and  the  title  bar's  close  button  even if no cancel button is
   specified  in  either the CommonButtons or Buttons members.
 */
METHOD AllowDialogCancellation( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_ALLOW_DIALOG_CANCELLATION ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Indicates that the task dialog can be minimized (read/write).
 */
METHOD CanBeMinimized( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_CAN_BE_MINIMIZED ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_CAN_BE_MINIMIZED )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_CAN_BE_MINIMIZED ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Whether to enable hyperlinks (read/write).

   NOTE:  Enables  hyperlink  processing  for  the  strings  specified  in the
   Content,  ExpandedInformation  and  Footer  members.  When  enabled,  these
   members may point to strings that contain hyperlinks in the following form:

   <A HREF="executablestring">Hyperlink Text</A>

   NOTE:  Task  Dialogs  will  not  actually execute any hyperlinks. Hyperlink
   execution _must be handled_ in the OnHyperlinkClicked event.

   WARNING:  Enabling  hyperlinks when using content from an unsafe source may
   cause security vulnerabilities.
 */
METHOD EnableHyperlinks( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_ENABLE_HYPERLINKS ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_ENABLE_HYPERLINKS )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_ENABLE_HYPERLINKS ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Indicates  that  the  string specified by the ExpandedInformation member is
   displayed when the dialog is initially displayed (read/write).

   NOTE: This flag is ignored if the ExpandedInformation member is empty.
 */
METHOD ExpandedByDefault( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_EXPANDED_BY_DEFAULT ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_EXPANDED_BY_DEFAULT )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_EXPANDED_BY_DEFAULT ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Whether expand footer area is displayed at the bottom (read/write).

   NOTE: Indicates that the string specified by the ExpandedInformation member
   is  displayed  at  the  bottom  of  the  dialog's  footer  area  instead of
   immediately  after  the  dialog's  content.  This  flag  is  ignored if the
   ExpandedInformation member is empty.
 */
METHOD ExpandFooterArea( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_EXPAND_FOOTER_AREA ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_EXPAND_FOOTER_AREA )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_EXPAND_FOOTER_AREA ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Indicates that no default item will be selected (read/write)
 */
METHOD NoDefaultRadioButton( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_NO_DEFAULT_RADIO_BUTTON ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Indicates  that  the  task  dialog is positioned (centered) relative to the
   window specified by parent.

   NOTE:  If  the flag is not supplied (or no parent member is specified), the
   task dialog is positioned (centered) relative to the monitor.
 */
METHOD PositionRelativeToWindow( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_POSITION_RELATIVE_TO_WINDOW ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   Indicates that text is displayed reading right to left (read/write).
 */
METHOD RightToLeftLayout( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_RTL_LAYOUT ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_RTL_LAYOUT )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_RTL_LAYOUT ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   The enable state of the verification checkbox.

   NOTE: Can be true to enable the checkbox or false to disable.
 */
METHOD VerificationEnabled( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_VERIFICATION_FLAG_CHECKED ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_VERIFICATION_FLAG_CHECKED )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_VERIFICATION_FLAG_CHECKED ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*
   The timeout for the dialog (read/write).

   NOTE: In Milliseconds. The dialog closes after given time.
 */
METHOD timeoutMS ( nMS ) CLASS TTaskDialog

   LOCAL nOldVal := ::nTimeOutMS

   IF ! ::lActive .AND. HB_ISNUMERIC( nMS )
      ::nTimeOutMS := nMS
   ENDIF

RETURN nOldVal

/*
   Whether we got a timeout (read/write, read only in future, maybe)
 */
METHOD TimedOut( lOut ) CLASS TTaskDialog

   IF ::lActive .AND. HB_ISLOGICAL( lOut )
      ::lTimeOut := lOut
   ENDIF

RETURN ::lTimeOut

#endif /* __XHARBOUR__ */
