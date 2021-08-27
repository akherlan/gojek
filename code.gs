// Change two necessary variables below:
// 1. label for email will be pulled 
var mailLabel = "Invoice/Gojek/Travel"
// 2. allowed email for security reason
var adminMail = "herlandi.27@gmail.com"

// Add Spreadsheet's UI menu
function onOpen(e) {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu("Pull mails").addItem("Get travel details", "getTravel").addToUi();
}

// Get mails
function getTravel() {

  // security
  if (Session.getActiveUser().getEmail() != adminMail) {
    Browser.msgBox("Please LogIn as a valid users!");
    return;
  }

  // define label and its thread
  var travelLabel = GmailApp.getUserLabelByName(mailLabel);
  var travelThread = travelLabel.getThreads();

  for(var i = travelThread.length-1; i >= 0; i--) {

    var travelMessage = travelThread[i].getMessages();

    for(var j = 0; j < travelMessage.length; j++) {
      var travelMessage = travelMessage[j];
      extractDetails(travelMessage);
    }
  }

}

// Write on Spreadsheets
function extractDetails(travelMessage) {
  
  // define fileds column
  var datetime = travelMessage.getDate();
  var subject = travelMessage.getSubject();
  var sender = travelMessage.getFrom();
  var content = travelMessage.getPlainBody();

  var activeSheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  activeSheet.appendRow([datetime, sender, subject, content]);

}

