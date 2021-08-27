// Filter all travel invoice from GOJEK, for example:
// Matches: from:(no-reply@invoicing.go-jek.com)
// Do this: Skip Inbox, Apply label "yourlabel"

// Change two necessary variables below:
// 1. yourlabel for email will be pulled 
var mailLabel = "Invoice/Gojek/Travel";
// 2. allowed email for security reason
var adminMail = "herlandi.27@gmail.com";

// Add Spreadsheet's UI menu
function onOpen(e) {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu("Pull mails").addItem("Get travel details", "getMsg").addToUi();
}

/*
function test(){
  const travelThread = GmailApp.getUserLabelByName(mailLabel).getThreads();
  Logger.log(travelMessage = travelThread[0].getMessages()[1].getPlainBody());

} */

// Get mails
function getMsg() {

  // security
  if (Session.getActiveUser().getEmail() != adminMail) {
    Browser.msgBox("Please Login as a valid users!");
    return;
  }

  // index mails suitable to given label and their threads
  var travelThread = GmailApp.getUserLabelByName(mailLabel).getThreads();
  
  for(var i = 0; i < travelThread.length; i++) {

    var travelMessage = travelThread[i].getMessages();

    for(var j = 0; j < travelMessage.length; j++) {

      var travelDetail = travelMessage[j];

      // define fileds column
      var datetime = travelDetail.getDate();
      var subject = travelDetail.getSubject();
      var sender = travelDetail.getFrom();
      var content = travelDetail.getPlainBody();

      // append to new rows
      var activeSheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
      activeSheet.appendRow([datetime, sender, subject, content]);

    }
  }
}

