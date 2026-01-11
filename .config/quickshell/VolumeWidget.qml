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

    // Get volume status
    Process {
        id: volumeCheck
        running: true
        command: ["sh", "-c", "wpctl get-volume @DEFAULT_AUDIO_SINK@"]

        stdout: SplitParser {
            onRead: data => {
                var output = data.trim();
                isMuted = output.includes("[MUTED]");
                var match = output.match(/Volume:\s+([\d.]+)/);
                if (match) {
                    var volumeFloat = parseFloat(match[1]);

                    // Special case: 1.0 should display as 100%
                    if (volumeFloat >= 1.0) {
                        volumeLevel = 100;
                    } else {
                        // Subtract 0.01 to compensate for PipeWire's logarithmic scale
                        var rawVolume = (volumeFloat - 0.01) * 100;
                        // Treat anything < 1.5% as 0% to prevent flickering
                        if (rawVolume < 1.5) {
                            volumeLevel = 0;
                        } else {
                            // Round to nearest 5% to match keyboard behavior
                            volumeLevel = Math.round(rawVolume / 5) * 5;
                        }
                    }
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
        command: ["sh", "-c", "wpctl set-volume --limit 1.0 @DEFAULT_AUDIO_SINK@ " + ((targetVolume + 1) / 100.0)]
    }

    // Volume change sound process
    Process {
        id: volumeSound
        command: ["canberra-gtk-play", "-i", "audio-volume-change", "-d", "changeVolume"]
    }

    // Refresh timer
    Timer {
        interval: 100
        running: true
        repeat: true
        onTriggered: {
            volumeCheck.running = false;
            volumeCheck.running = true;
        }
    }


    Rectangle {
        id: mainWidget
        anchors.fill: parent
        color: "#6663a0"

        // Sunken/inset border effect (dark on top/left, light on bottom/right)
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

            onWheel: wheel => {
                // Positive angleDelta.y means scroll up (increase volume)
                // Negative angleDelta.y means scroll down (decrease volume)
                var delta = wheel.angleDelta.y > 0 ? 5 : -5;
                var newVolume = Math.max(0, Math.min(100, volumeLevel + delta));

                volumeLevel = newVolume;
                volumeSet.targetVolume = newVolume;
                volumeSet.running = false;
                volumeSet.running = true;

                // Play volume change sound
                volumeSound.running = false;
                volumeSound.running = true;
            }
        }

        Canvas {
            id: volumeCanvas
            anchors.fill: parent
            anchors.margins: 4

            onPaint: {
                var ctx = getContext("2d");
                ctx.clearRect(0, 0, width, height);

                var centerX = width / 2;
                var centerY = height / 2;

                // Scale icon relative to container height (0.8 = 80% of container height)
                var iconScale = height * 0.8;

                // Speaker dimensions
                var speakerBoxWidth = iconScale * 0.3;
                var speakerBoxHeight = iconScale * 0.625;
                var speakerConeWidth = iconScale * 0.5;
                var speakerConeHeight = iconScale * 1.2;

                // Wave dimensions
                var coneEndX = speakerConeWidth;
                var wave1Radius = iconScale * 0.4375;
                var wave2Radius = iconScale * 0.625;
                var wave3Radius = iconScale * 0.8125;

                // Calculate icon positioning to center the entire icon including all waves
                var iconLeftEdge = -speakerBoxWidth;
                var iconRightEdge = coneEndX + wave3Radius;
                var iconCenterOffset = (iconLeftEdge + iconRightEdge) / 2;
                var refX = centerX - iconCenterOffset;

                // Draw large semi-transparent speaker icon in background
                ctx.fillStyle = "rgba(255, 255, 255, 0.15)";
                ctx.strokeStyle = "rgba(255, 255, 255, 0.15)";
                ctx.lineWidth = 3;

                // Speaker box (rectangle)
                ctx.fillRect(refX - speakerBoxWidth, centerY - speakerBoxHeight/2, speakerBoxWidth, speakerBoxHeight);

                // Speaker cone (trapezoid)
                ctx.beginPath();
                ctx.moveTo(refX, centerY - speakerBoxHeight/2);
                ctx.lineTo(refX + coneEndX, centerY - speakerConeHeight/2);
                ctx.lineTo(refX + coneEndX, centerY + speakerConeHeight/2);
                ctx.lineTo(refX, centerY + speakerBoxHeight/2);
                ctx.closePath();
                ctx.fill();

                // Sound waves or mute indicator
                if (!isMuted) {
                    ctx.lineWidth = 3;
                    ctx.lineCap = "round";

                    // Volume level determines number of waves
                    if (volumeLevel > 0) {
                        ctx.beginPath();
                        ctx.arc(refX + coneEndX, centerY, wave1Radius, -Math.PI/4, Math.PI/4, false);
                        ctx.stroke();
                    }

                    if (volumeLevel > 33) {
                        ctx.beginPath();
                        ctx.arc(refX + coneEndX, centerY, wave2Radius, -Math.PI/4, Math.PI/4, false);
                        ctx.stroke();
                    }

                    if (volumeLevel > 66) {
                        ctx.beginPath();
                        ctx.arc(refX + coneEndX, centerY, wave3Radius, -Math.PI/4, Math.PI/4, false);
                        ctx.stroke();
                    }
                } else {
                    // Draw X for muted
                    var muteXSize = iconScale * 0.7;
                    var muteXOffset = coneEndX + iconScale * 0.2;
                    ctx.lineWidth = 3;
                    ctx.beginPath();
                    ctx.moveTo(refX + muteXOffset, centerY - muteXSize/2);
                    ctx.lineTo(refX + muteXOffset + muteXSize, centerY + muteXSize/2);
                    ctx.moveTo(refX + muteXOffset + muteXSize, centerY - muteXSize/2);
                    ctx.lineTo(refX + muteXOffset, centerY + muteXSize/2);
                    ctx.stroke();
                }

                // Draw percentage or muted text prominently in center
                ctx.font = "bold 14px Monospace";
                ctx.fillStyle = "#ffffff";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText(isMuted ? "Muted" : volumeLevel + "%", centerX, centerY);
            }
        }
    }

    // Trigger repaint when volume or mute state changes
    onVolumeLevelChanged: volumeCanvas.requestPaint()
    onIsMutedChanged: volumeCanvas.requestPaint()
}
