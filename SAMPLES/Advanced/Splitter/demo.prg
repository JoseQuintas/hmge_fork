/*
 * Harbour MiniGUI Splitter Demo
 */
ANNOUNCE RDDSYS

#include "minigui.ch"

PROCEDURE Main()

    LOCAL VerticalSplitter, HorizontalSplitter

    // Enable automatic resizing of controls when the window is resized
    SET AUTOADJUST ON

    // Define the main window
    DEFINE WINDOW MainWindow ;
        AT 0, 0 ;
        WIDTH 800 ;
        HEIGHT 600 ;
        TITLE "Vertical and Horizontal SPLITTER Demo" ;
        MAIN

        // Define two EditBoxes for the vertical splitter
        @ 50, 50 EDITBOX LeftEditBox ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Left Pane" ;
            NOVSCROLL NOHSCROLL

        @ 50, 380 EDITBOX RightEditBox ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Right Pane" ;
            NOVSCROLL NOHSCROLL

        // Define a VERTICAL SPLITTER between the two EditBoxes
        DEFINE SPLITTER VerticalSplitter ;
            AT 50, 370 ;                  // Position the splitter between controls
            WIDTH 10 ;                    // Width of the splitter
            HEIGHT 250 ;                  // Height of the splitter
            VERTICAL ;                    // Vertical splitter
            SPLIT {"LeftEditBox"} FROM {"RightEditBox"} ;  // Split the two controls
            COLOR {0, 128, 255} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {0, 128, 255}, {0, 0, 128} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {200, 50, 50}, {100, 0, 0} ;  // Hover gradient colors
            HIDEARROW ;                   // Hide the arrow icon
            LIMITS {5, 10}                // Define the minimum and maximum limits for each section

        // Define two more EditBoxes for the horizontal splitter
        @ 320, 50 EDITBOX TopEditBox ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Top Pane" ;
            NOVSCROLL NOHSCROLL

        @ 430, 50 EDITBOX BottomEditBox ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Bottom Pane" ;
            NOVSCROLL NOHSCROLL

        // Define a HORIZONTAL SPLITTER between the two EditBoxes
        DEFINE SPLITTER HorizontalSplitter ;
            AT 420, 50 ;                  // Position the splitter between controls
            WIDTH 650 ;                   // Width of the splitter
            HEIGHT 10 ;                   // Height of the splitter
            HORIZONTAL ;                  // Horizontal splitter
            SPLIT {"TopEditBox"} FROM {"BottomEditBox"} ;  // Split the two controls
            COLOR {255, 128, 0} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {255, 128, 0}, {255, 64, 0} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {255, 100, 100}, {200, 0, 0} ;  // Hover gradient colors
            LIMITS {5, 15}                // Define the minimum and maximum limits for each section

    END WINDOW

    // Center the main window
    CENTER WINDOW MainWindow

    // Activate the main window
    ACTIVATE WINDOW MainWindow

RETURN
