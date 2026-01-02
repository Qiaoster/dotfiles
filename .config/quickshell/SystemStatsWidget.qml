import Quickshell
import Quickshell.Io
import QtQuick
import QtQuick.Layouts

RowLayout {
    spacing: 8

    property var cpuHistory: []
    property var memHistory: []
    property int maxHistoryLength: 30

    // CPU Usage
    CdePanel {
        Layout.preferredWidth: 90
        Layout.fillHeight: true

        Canvas {
            id: cpuCanvas
            anchors.fill: parent
            anchors.margins: 4

            onPaint: {
                var ctx = getContext("2d");
                ctx.clearRect(0, 0, width, height);

                // Draw background text
                ctx.font = "bold 32px Sans";
                ctx.fillStyle = "rgba(255, 255, 255, 0.15)";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText("CPU", width / 2, height / 2 + 3);

                // Draw graph
                if (cpuHistory.length < 2) return;

                ctx.strokeStyle = "#5eb3ff";
                ctx.lineWidth = 2;
                ctx.beginPath();

                var stepX = width / (maxHistoryLength - 1);
                var startIdx = Math.max(0, cpuHistory.length - maxHistoryLength);

                var lastX, lastY;
                for (var i = 0; i < cpuHistory.length - startIdx; i++) {
                    var value = cpuHistory[startIdx + i];
                    var x = i * stepX;
                    var y = height - (value / 100 * height);

                    if (i === 0) {
                        ctx.moveTo(x, y);
                    } else {
                        ctx.lineTo(x, y);
                    }

                    lastX = x;
                    lastY = y;
                }

                ctx.stroke();

                // Draw percentage at head of line
                ctx.font = "bold 11px Sans";
                ctx.fillStyle = "#5eb3ff";
                ctx.textAlign = "right";
                ctx.textBaseline = "bottom";
                ctx.fillText(cpuUsage + "%", lastX - 4, lastY);
            }
        }
    }

    // Memory Usage
    CdePanel {
        Layout.preferredWidth: 90
        Layout.fillHeight: true

        Canvas {
            id: memCanvas
            anchors.fill: parent
            anchors.margins: 4

            onPaint: {
                var ctx = getContext("2d");
                ctx.clearRect(0, 0, width, height);

                // Draw background text
                ctx.font = "bold 32px Sans";
                ctx.fillStyle = "rgba(255, 255, 255, 0.15)";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText("MEM", width / 2, height / 2 + 3);

                // Draw graph
                if (memHistory.length < 2) return;

                ctx.strokeStyle = "#ffb366";
                ctx.lineWidth = 2;
                ctx.beginPath();

                var stepX = width / (maxHistoryLength - 1);
                var startIdx = Math.max(0, memHistory.length - maxHistoryLength);

                var lastX, lastY;
                for (var i = 0; i < memHistory.length - startIdx; i++) {
                    var value = memHistory[startIdx + i];
                    var x = i * stepX;
                    var y = height - (value / 100 * height);

                    if (i === 0) {
                        ctx.moveTo(x, y);
                    } else {
                        ctx.lineTo(x, y);
                    }

                    lastX = x;
                    lastY = y;
                }

                ctx.stroke();

                // Draw percentage at head of line
                ctx.font = "bold 11px Sans";
                ctx.fillStyle = "#ffb366";
                ctx.textAlign = "right";
                ctx.textBaseline = "bottom";
                ctx.fillText(memUsage + "%", lastX - 4, lastY);
            }
        }
    }

    property int cpuUsage: 0
    property int memUsage: 0

    // CPU monitoring
    Process {
        id: cpuProcess
        running: true
        command: ["sh", "-c", "top -bn2 -d 0.5 | grep '^%Cpu' | tail -1 | awk '{print 100-$8}'"]

        stdout: SplitParser {
            onRead: data => {
                var usage = parseFloat(data.trim());
                if (!isNaN(usage)) {
                    cpuUsage = Math.round(usage);

                    // Add to history
                    var newHistory = cpuHistory.slice();
                    newHistory.push(cpuUsage);
                    if (newHistory.length > maxHistoryLength) {
                        newHistory.shift();
                    }
                    cpuHistory = newHistory;

                    // Trigger repaint
                    cpuCanvas.requestPaint();
                }
            }
        }
    }

    // Memory monitoring
    Process {
        id: memProcess
        running: true
        command: ["sh", "-c", "free | grep Mem | awk '{printf \"%.0f\", ($3/$2) * 100}'"]

        stdout: SplitParser {
            onRead: data => {
                var usage = parseInt(data.trim());
                if (!isNaN(usage)) {
                    memUsage = usage;

                    // Add to history
                    var newHistory = memHistory.slice();
                    newHistory.push(memUsage);
                    if (newHistory.length > maxHistoryLength) {
                        newHistory.shift();
                    }
                    memHistory = newHistory;

                    // Trigger repaint
                    memCanvas.requestPaint();
                }
            }
        }
    }

    // Update timer
    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: {
            cpuProcess.running = false;
            memProcess.running = false;
            cpuProcess.running = true;
            memProcess.running = true;
        }
    }

    component CdePanel: Rectangle {
        color: "#6663a0"

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
    }
}
