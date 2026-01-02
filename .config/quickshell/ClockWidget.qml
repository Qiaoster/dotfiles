import QtQuick

Rectangle {
    implicitWidth: timeText.width + 16
    implicitHeight: timeText.height + 8
    color: "#6663a0"

    // Inset border effect (sunken panel)
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

    Text {
        id: timeText
        anchors.centerIn: parent
        text: Time.time
        font.family: "Courier"
        font.pixelSize: 15
        font.weight: Font.Bold
        color: "#ffffff"
    }
}
