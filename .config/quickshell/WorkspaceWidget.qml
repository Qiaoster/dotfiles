import Quickshell
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

RowLayout {
    spacing: 2

    Repeater {
        model: Hyprland.workspaces

        Rectangle {
            required property var modelData

            // Show workspace (Hyprland.workspaces only includes occupied ones)
            Layout.preferredWidth: 36
            Layout.fillHeight: true
            Layout.margins: 2

            property bool isActive: {
                if (!hyprlandFocusedMonitor || !hyprlandFocusedMonitor.activeWorkspace) {
                    return false;
                }
                return hyprlandFocusedMonitor.activeWorkspace.id === modelData.id;
            }

            // CDE-style button with 3D effect - Solaris 8 colors
            color: isActive ? "#524c8a" : "#9694c4"
            border.width: 0

            // Top-left highlight (raised/pressed button)
            Rectangle {
                anchors {
                    top: parent.top
                    left: parent.left
                    right: parent.right
                }
                height: 2
                color: parent.isActive ? "#3d3866" : "#AEB2C3"
            }

            Rectangle {
                anchors {
                    top: parent.top
                    left: parent.left
                    bottom: parent.bottom
                }
                width: 2
                color: parent.isActive ? "#3d3866" : "#AEB2C3"
            }

            // Bottom-right shadow
            Rectangle {
                anchors {
                    bottom: parent.bottom
                    left: parent.left
                    right: parent.right
                }
                height: 2
                color: parent.isActive ? "#AEB2C3" : "#3d3866"
            }

            Rectangle {
                anchors {
                    top: parent.top
                    right: parent.right
                    bottom: parent.bottom
                }
                width: 2
                color: parent.isActive ? "#AEB2C3" : "#3d3866"
            }

            Text {
                anchors.centerIn: parent
                text: modelData.id === 10 ? "0" : modelData.id.toString()
                font.family: "Helvetica"
                font.pixelSize: 14
                font.weight: Font.Bold
                color: "#ffffff"
            }

            MouseArea {
                anchors.fill: parent
                onClicked: {
                    Hyprland.dispatch("workspace " + modelData.id.toString());
                }
            }
        }
    }

    property var hyprlandFocusedMonitor: Hyprland.focusedMonitor
}
