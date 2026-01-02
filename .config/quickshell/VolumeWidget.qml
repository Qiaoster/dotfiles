import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

Item {
    id: root
    implicitWidth: 60
    implicitHeight: 40

    required property var panelWindow

    property int volumeLevel: 0
    property bool isMuted: false
    property bool isDragging: false
    property bool showSlider: mainMouseArea.containsMouse || sliderArea.containsMouse || sliderMouseArea.containsMouse || isDragging

    // Get volume status
    Process {
        id: volumeCheck
        running: true
        command: ["sh", "-c", "wpctl get-volume @DEFAULT_AUDIO_SINK@"]

        stdout: SplitParser {
            onRead: data => {
                if (isDragging) return;
                var output = data.trim();
                isMuted = output.includes("[MUTED]");
                var match = output.match(/Volume:\s+([\d.]+)/);
                if (match) {
                    volumeLevel = Math.round(parseFloat(match[1]) * 100);
                }
            }
        }
    }

    // Mute toggle process
    Process {
        id: muteToggle
        command: ["sh", "-c", "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"]
    }

    // Volume set process
    Process {
        id: volumeSet
        property int targetVolume: 50
        command: ["sh", "-c", "wpctl set-volume @DEFAULT_AUDIO_SINK@ " + (targetVolume / 100.0)]
    }

    // Audio feedback process
    Process {
        id: audioFeedback
        command: ["canberra-gtk-play", "-i", "audio-volume-change", "-d", "changeVolume"]
    }

    // Refresh timer
    Timer {
        interval: 100
        running: true
        repeat: true
        onTriggered: {
            if (!isDragging) {
                volumeCheck.running = false;
                volumeCheck.running = true;
            }
        }
    }


    Rectangle {
        id: mainWidget
        anchors.fill: parent
        color: "#6663a0"

        // Inset border effect
        Rectangle {
            anchors {
                top: parent.top
                left: parent.left
                right: parent.right
            }
            height: 1
            color: "#3d3866"
        }

        Rectangle {
            anchors {
                top: parent.top
                left: parent.left
                bottom: parent.bottom
            }
            width: 1
            color: "#3d3866"
        }

        Rectangle {
            anchors {
                bottom: parent.bottom
                left: parent.left
                right: parent.right
            }
            height: 1
            color: "#AEB2C3"
        }

        Rectangle {
            anchors {
                top: parent.top
                right: parent.right
                bottom: parent.bottom
            }
            width: 1
            color: "#AEB2C3"
        }

        MouseArea {
            id: mainMouseArea
            anchors.fill: parent
            hoverEnabled: true
            onClicked: {
                muteToggle.running = true;
            }
        }

        RowLayout {
            anchors.fill: parent
            anchors.margins: 4
            spacing: 4

            // Volume icon
            Rectangle {
                width: 24
                height: 24
                color: isMuted ? "#666" : "#9694c4"
                border.color: "#AEB2C3"
                border.width: 1

                Text {
                    anchors.centerIn: parent
                    text: isMuted ? "🔇" : (volumeLevel > 50 ? "🔊" : (volumeLevel > 0 ? "🔉" : "🔈"))
                    font.pixelSize: 14
                }
            }

            // Volume percentage
            Text {
                text: volumeLevel + "%"
                font.family: "Courier"
                font.pixelSize: 11
                color: "#ffffff"
            }
        }
    }

    // Popup slider window
    PopupWindow {
        id: sliderPopup
        visible: showSlider || isDragging

        anchor.window: panelWindow
        anchor.rect.x: {
            // Explicitly track root.x to ensure binding updates
            var baseX = root.x;
            var pos = root.mapToItem(null, 0, 0);
            return pos.x + root.width / 2 - width / 2;
        }
        anchor.rect.y: {
            var pos = root.mapToItem(null, 0, 0);
            return pos.y + root.height + 4;
        }

        implicitWidth: 40
        implicitHeight: 150

        Rectangle {
            anchors.fill: parent
            color: "#7c7bb8"

            // Border
            Rectangle {
                anchors {
                    top: parent.top
                    left: parent.left
                    right: parent.right
                }
                height: 2
                color: "#AEB2C3"
            }

            Rectangle {
                anchors {
                    top: parent.top
                    left: parent.left
                    bottom: parent.bottom
                }
                width: 2
                color: "#AEB2C3"
            }

            Rectangle {
                anchors {
                    bottom: parent.bottom
                    left: parent.left
                    right: parent.right
                }
                height: 2
                color: "#3d3866"
            }

            Rectangle {
                anchors {
                    top: parent.top
                    right: parent.right
                    bottom: parent.bottom
                }
                width: 2
                color: "#3d3866"
            }

            MouseArea {
                id: sliderArea
                anchors.fill: parent
                hoverEnabled: true
            }

            Item {
                anchors.fill: parent
                anchors.margins: 8

                // Volume percentage at top
                Text {
                    anchors {
                        top: parent.top
                        horizontalCenter: parent.horizontalCenter
                    }
                    text: volumeLevel + "%"
                    font.family: "Courier"
                    font.pixelSize: 12
                    font.weight: Font.Bold
                    color: "#ffffff"
                }

                // Slider track (vertical, sunken)
                Rectangle {
                    anchors {
                        top: parent.top
                        bottom: parent.bottom
                        horizontalCenter: parent.horizontalCenter
                        topMargin: 20
                    }
                    width: 8
                    color: "#524c8a"

                    // Inset effect
                    Rectangle {
                        anchors {
                            top: parent.top
                            left: parent.left
                            bottom: parent.bottom
                        }
                        width: 1
                        color: "#3d3866"
                    }

                    Rectangle {
                        anchors {
                            top: parent.top
                            left: parent.left
                            right: parent.right
                        }
                        height: 1
                        color: "#3d3866"
                    }

                    // Filled portion (from bottom up)
                    Rectangle {
                        anchors {
                            left: parent.left
                            right: parent.right
                            bottom: parent.bottom
                        }
                        height: parent.height * (volumeLevel / 100)
                        color: isMuted ? "#666" : "#AEB2C3"
                    }

                    // Slider handle
                    Rectangle {
                        y: Math.max(0, Math.min(parent.height - height, parent.height - (parent.height * (volumeLevel / 100)) - height / 2))
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: 20
                        height: 16
                        color: "#9694c4"

                        // 3D effect
                        Rectangle {
                            anchors {
                                top: parent.top
                                left: parent.left
                                right: parent.right
                            }
                            height: 1
                            color: "#AEB2C3"
                        }

                        Rectangle {
                            anchors {
                                top: parent.top
                                left: parent.left
                                bottom: parent.bottom
                            }
                            width: 1
                            color: "#AEB2C3"
                        }

                        Rectangle {
                            anchors {
                                bottom: parent.bottom
                                left: parent.left
                                right: parent.right
                            }
                            height: 1
                            color: "#3d3866"
                        }

                        Rectangle {
                            anchors {
                                top: parent.top
                                right: parent.right
                                bottom: parent.bottom
                            }
                            width: 1
                            color: "#3d3866"
                        }
                    }

                    MouseArea {
                        id: sliderMouseArea
                        anchors.fill: parent
                        hoverEnabled: true

                        onPressed: mouse => {
                            isDragging = true;
                            updateVolume(mouse.y);
                        }

                        onPositionChanged: mouse => {
                            if (pressed) {
                                updateVolume(mouse.y);
                            }
                        }

                        onReleased: {
                            isDragging = false;
                            volumeCheck.running = false;
                            volumeCheck.running = true;
                            audioFeedback.running = false;
                            audioFeedback.running = true;
                        }

                        function updateVolume(mouseY) {
                            // Inverted: top = 100%, bottom = 0%
                            var newVolume = Math.max(0, Math.min(100, Math.round((1 - mouseY / height) * 100)));
                            volumeLevel = newVolume;
                            volumeSet.targetVolume = newVolume;
                            volumeSet.running = false;
                            volumeSet.running = true;
                        }
                    }
                }
            }
        }
    }
}
