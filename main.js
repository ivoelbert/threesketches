const electron = require('electron')
// Module to control application life.
const app = electron.app
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow
const {ipcMain} = require('electron');


const path = require('path');
const fs = require('fs');
const url = require('url');

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;

function createWindow () {
  // Create the browser window.
  mainWindow = new BrowserWindow({width: 800, height: 800, resizable: false, toolbar: false, icon:  "resourses/images/icon.png"})

  // and load the index.html of the app.
  mainWindow.loadURL(url.format({
    pathname: path.join(__dirname, 'index.html'),
    protocol: 'file:',
    slashes: true
  }))
  mainWindow.setMenuBarVisibility(false);

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()

  // Emitted when the window is closed.
  mainWindow.on('closed', function () {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}


// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow); // Carga la base de datos, una vez cargada crea la ventana.

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

// Evento que se va a encargar de hacer lo que quiera con los datos.

  /************************** SCAF **************************/
  /*   guardar los formularios en la bd sobre el filesys    */
  /**********************************************************/
/*
ipcMain.on('saveData', (event, data) => {
  fileHandler.saveDataToDB(data);
})
*/

// Evento que se encarga de obtener datos y enviarlos.
ipcMain.on('requestData', (event, item) =>{

  fs.readFile( item.path, (err, data) => {

      if (err){//del path obtengo el id de estudio, la key es el nombre del guachin
        let id = item.path.split('/')[2].split('.')[0];
        fileHandler.removeItem(id , item.key ).then( (data) => {
          event.sender.send('clearEstudios', data);
        });

      }else{
        event.sender.send('getData', JSON.parse(data));
      }


  });

})
