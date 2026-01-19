import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts

Scope {
    property color color_bg: "#7c7bb8"
    property color color_bg_dark: "#6663a0"

    property color color_text: "#ffffff"
    property color color_border_light: "#aeb2c3"
    property color color_border_dark: "#3d3866"
    property color color_item_light: "#524c8a"
    property color color_item_dark: "#9694c4"

    Variants {
        model: Quickshell.screens;

        PanelWindow {
            id: panel
            required property var modelData
            screen:modelData

            anchors {
                top: true
                left: true
                right: true
            }

            property int bar_height: Math.max(modelData.height, modelData.width) / 40
            property int bar_margin: bar_height / 20

            exclusiveZone: bar_height
            implicitHeight: bar_height
            focusable: true

            Rectangle {
                anchors.fill: parent
                color: color_bg

                // Top border, bright
                Rectangle {
                    anchors {
                        top: parent.top
                        left: parent.left
                        right: parent.right
                    }
                    height: bar_margin
                    color: color_border_light
                }

                // Bottom border, dark
                Rectangle {
                    anchors {
                        bottom: parent.bottom
                        left: parent.left
                        right: parent.right
                    }
                    height: bar_margin
                    color: color_border_dark
                }
            }

            // Workspaces
            RowLayout {
                anchors {
                    left: parent.left
                    top: parent.top
                    bottom: parent.bottom
                    margins: bar_margin * 2
                }
                spacing: bar_margin
                Repeater {
                    model: Hyprland.workspaces

                    Rectangle {
                        required property var modelData
                        property var focusedMonitor: Hyprland.focusedMonitor

                        Layout.preferredWidth: bar_height - 4 * bar_margin
                        Layout.fillHeight: true
                        Layout.margins: bar_margin

                        property bool isActive: {
                            if (!focusedMonitor || !focusedMonitor.activeWorkspace) {
                                return false;
                            }
                            return focusedMonitor.activeWorkspace.id === modelData.id;
                        }

                        color: isActive ? color_item_light : color_item_dark
                        Behavior on color {
                            ColorAnimation { duration: 100; easing.type: Easing.InOutQuad }
                        }

                        Bevel {
                            anchors.fill: parent
                            up_left_color: parent.isActive ? color_border_dark : color_border_light
                            bottom_right_color: parent.isActive ? color_border_light : color_border_dark
                            thickness: bar_margin
                        }

                        Text {
                            anchors.centerIn: parent
                            text: modelData.id == 10 ? "0" : modelData.id.toString()
                            font.family: "Helvetica"
                            font.pixelSize: 14
                            font.weight: Font.Bold
                            color: color_text
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: Hyprland.dispatch("workspace " + modelData.id.toString())
                        }
                    }
                }

            }

            // Clock
            Rectangle {
                color: color_bg_dark
                anchors.centerIn: parent

                Bevel {
                    anchors.fill: parent
                    up_left_color: color_border_dark
                    bottom_right_color: color_border_light
                }
                Text {
                    id: timeText
                    anchors.centerIn: parent
                    text: ""
                    font.family: "JetBrainsMono Nerd Font"
                    font.pixelSize: bar_height / 3
                    font.weight: Font.Bold
                    color: color_text

                    function updateTime() {
                        onTriggered: timeText.text = Qt.formatDateTime(new Date(), "ddd MMM d  hh:mm:ss AP")
                    }

                    Timer {
                        interval: 1000
                        running: true
                        repeat: true
                        onTriggered: timeText.updateTime()
                    }
                    Component.onCompleted: updateTime()
                }
                implicitWidth: timeText.width + 6 * bar_margin
                implicitHeight: timeText.height + 2 * bar_margin
            }

            // Volume Control
            Item {
                id: volume_widget
                implicitHeight: bar_height - 4 * bar_margin
                implicitWidth: 1.5 * height
                anchors {
                    top: parent.top
                    bottom: parent.bottom
                    right: parent.right
                    margins: bar_margin * 2
                }

                property bool muted: false
                property int volumeLevel: 0

                // Get volume status
                Process {
                    id: getVolume
                    running: true
                    command: ["sh", "-c", "wpctl get-volume @DEFAULT_AUDIO_SINK@"]

                    stdout: SplitParser {
                        onRead: data => {
                            var output = data.trim();
                            volume_widget.muted = output.includes("[MUTED]");
                            var match = output.match(/Volume:\s+([\d.]+)/);
                            if (match) {
                                var volumeFloat = parseFloat(match[1]);
                                if (volumeFloat >= 1.0) {
                                    volume_widget.volumeLevel = 100;
                                } else if (volumeFloat <= 0.011) {
                                    volume_widget.volumeLevel = 0;
                                } else {
                                    volume_widget.volumeLevel = volumeFloat * 100;
                                }
                            }
                        }
                    }
                }

                // Refresh display
                Timer {
                    interval: 100
                    running: true
                    repeat: true
                    onTriggered: {
                        getVolume.running = false
                        getVolume.running = true
                    }
                }

                // Toggle mute
                Process {
                    id: toggleMute
                    command: ["wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"]
                }

                // Change volume
                Process {
                    id: changeVolume
                    property int targetVolume: 0
                    command: ["sh", "-c", "wpctl set-volume --limit 1.0 @DEFAULT_AUDIO_SINK@ " + (targetVolume / 100.0)]
                }

                // Play volume indicator sound
                Process {
                    id: volumeDing
                    command: ["paplay", "/home/qiao/.config/quickshell/volumeDing.wav"]
                }

                Rectangle {
                    anchors.fill: parent
                    color: color_bg_dark
                    Bevel {
                        anchors.fill: parent
                        up_left_color: color_border_dark
                        bottom_right_color: color_border_light
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true

                        onClicked: {
                            toggleMute.running = true;
                        }

                        onWheel: wheel=> {
                            // Positive angleDelta.y means scroll up (increase volume)
                            // Negative angleDelta.y means scroll down (decrease volume)
                            var delta = wheel.angleDelta.y > 0 ? 5 : -5;
                            var newVolume = Math.max(0, Math.min(100, volume_widget.volumeLevel + delta));
                            if (newVolume <= 1) newVolume = 0;

                            volume_widget.volumeLevel = newVolume;
                            changeVolume.targetVolume = newVolume;
                            changeVolume.running = false;
                            changeVolume.running = true;

                            // Play volume change sound
                            volumeDing.running = false;
                            volumeDing.running = true;
                        }
                    }

                    // Visuals
                    Canvas {
                        id: volumeCanvas
                        anchors.fill: parent
                        anchors.margins: 2 * bar_margin
                        onPaint: {
                            var context = getContext("2d");
                            context.clearRect(0,0,width,height);

                            var centerX = width / 2;
                            var centerY = height / 2;

                            // scale icon to 80% of container height
                            var iconScale = 0.8
                            var iconSize = height * iconScale

                            // Speaker shape dimensions
                            var speakerBoxWidth = iconSize * 0.3;
                            var speakerBoxHeight = iconSize * 0.625;
                            var speakerConeWidth = iconSize * 0.5;
                            var speakerConeHeight = iconSize * 1.2;

                            // Wave shape dimensions
                            var coneEndX = speakerConeWidth;
                            var wave1Radius = iconSize * 0.4375;
                            var wave2Radius = iconSize * 0.625;
                            var wave3Radius = iconSize * 0.8125;
                            var waveWidth = bar_margin * 1.5

                            // Icon positioning
                            var iconLeftEdge = -speakerBoxWidth;
                            var iconRightEdge = coneEndX + wave3Radius;
                            var iconCenterOffset = (iconLeftEdge + iconRightEdge) / 2;
                            var refX = centerX - iconCenterOffset;

                            // Draw large semi-transparent speaker icon in background
                            context.fillStyle = "rgba(255, 255, 255, 0.2)";
                            context.strokeStyle = "rgba(255, 255, 255, 0.2)";
                            context.lineWidth = bar_margin * 1.5;

                            // Speaker box (rectangle)
                            context.fillRect(refX - speakerBoxWidth, centerY - speakerBoxHeight/2, speakerBoxWidth, speakerBoxHeight);

                            // Speaker cone (trapezoid)
                            context.beginPath();
                            context.moveTo(refX, centerY - speakerBoxHeight/2);
                            context.lineTo(refX + coneEndX, centerY - speakerConeHeight/2);
                            context.lineTo(refX + coneEndX, centerY + speakerConeHeight/2);
                            context.lineTo(refX, centerY + speakerBoxHeight/2);
                            context.closePath();
                            context.fill();

                            // Sound waves or mute indicator
                            if (!volume_widget.muted) {
                                context.lineWidth = waveWidth;
                                context.lineCap = "round";

                                // Volume level determines number of waves
                                if (volume_widget.volumeLevel > 0) {
                                    context.beginPath();
                                    context.arc(refX + coneEndX, centerY, wave1Radius, -Math.PI/6, Math.PI/6, false);
                                    context.stroke();
                                }

                                if (volume_widget.volumeLevel > 33) {
                                    context.beginPath();
                                    context.arc(refX + coneEndX, centerY, wave2Radius, -Math.PI/6, Math.PI/6, false);
                                    context.stroke();
                                }

                                if (volume_widget.volumeLevel > 66) {
                                    context.beginPath();
                                    context.arc(refX + coneEndX, centerY, wave3Radius, -Math.PI/6, Math.PI/6, false);
                                    context.stroke();
                                }
                            } else {
                                // Draw X for muted
                                var muteXSize = iconSize * 0.7;
                                var muteXOffset = coneEndX + iconSize * 0.2;
                                context.lineWidth = bar_margin * 1.5;
                                context.beginPath();
                                context.moveTo(refX + muteXOffset, centerY - muteXSize/2);
                                context.lineTo(refX + muteXOffset + muteXSize, centerY + muteXSize/2);
                                context.moveTo(refX + muteXOffset + muteXSize, centerY - muteXSize/2);
                                context.lineTo(refX + muteXOffset, centerY + muteXSize/2);
                                context.stroke();
                            }

                            // Draw percentage or muted text prominently in center
                            context.font = "bold 14px Monospace";
                            context.fillStyle = color_text;
                            context.textAlign = "center";
                            context.textBaseline = "middle";
                            context.fillText(volume_widget.muted ? "Muted" : volume_widget.volumeLevel + "%", centerX, centerY);
                        }
                    }
                }

                onVolumeLevelChanged: volumeCanvas.requestPaint()
                onMutedChanged: volumeCanvas.requestPaint()
            }
        }
    }
}
