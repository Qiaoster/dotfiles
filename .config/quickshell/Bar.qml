import Quickshell
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

Scope {
    Variants {
        model: Quickshell.screens;

        PanelWindow {
            id: panel
            required property var modelData
            screen: modelData

            anchors {
                top: true
                left: true
                right: true
            }

            exclusiveZone: 48
            implicitHeight: 48
            focusable: true

            // CDE-style background with 3D raised effect - Solaris 8 colors
            Rectangle {
                anchors.fill: parent
                color: "#7c7bb8"

                // Top highlight border (light)
                Rectangle {
                    anchors {
                        top: parent.top
                        left: parent.left
                        right: parent.right
                    }
                    height: 2
                    color: "#AEB2C3"
                }

                // Bottom shadow border (dark)
                Rectangle {
                    anchors {
                        bottom: parent.bottom
                        left: parent.left
                        right: parent.right
                    }
                    height: 2
                    color: "#3d3866"
                }

                // Main content layout
                RowLayout {
                    anchors.fill: parent
                    anchors.margins: 4
                    spacing: 4

                    // Left section - Workspaces
                    WorkspaceWidget {
                        Layout.fillHeight: true
                    }

                    // Separator
                    CdeSeparator {}

                    // Center spacer
                    Item {
                        Layout.fillWidth: true
                    }

                    // Clock in center-right
                    ClockWidget {
                        Layout.alignment: Qt.AlignVCenter
                    }

                    // Right spacer
                    Item {
                        Layout.fillWidth: true
                    }

                    // System stats section
                    SystemStatsWidget {
                        Layout.fillHeight: true
                    }

                    // Separator
                    CdeSeparator {}

                    // Volume control
                    VolumeWidget {
                        Layout.fillHeight: true
                        panelWindow: panel
                    }
                }
            }
        }
    }

    // CDE-style separator component
    component CdeSeparator: Rectangle {
        width: 3
        Layout.fillHeight: true
        color: "transparent"

        Rectangle {
            anchors {
                left: parent.left
                top: parent.top
                bottom: parent.bottom
            }
            width: 1
            color: "#3d3866"
        }

        Rectangle {
            anchors {
                left: parent.left
                top: parent.top
                bottom: parent.bottom
                leftMargin: 1
            }
            width: 1
            color: "#AEB2C3"
        }
    }
}
