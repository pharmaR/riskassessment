//Adding javascript code for dashbord screen

var elementNavbarText = document.getElementsByClassName('myClass');
elementNavbarText[0].setAttribute('style', 'margin-left: -30% !important');


var elementNavbar = document.getElementsByClassName('navbar-static-top');
elementNavbar[0].style.justifyContent = 'center';


var elementHelpIconHeader = document.getElementsByClassName('asmnt-help-icon');
elementHelpIconHeader[0].style.display = 'block';


var elementSidebar = document.getElementsByClassName('sidebar-menu');


setTimeout(function() {
  document.getElementsByClassName('sidebar-menu')[0].addEventListener("click", mobileMenuClicked, false);
  document.getElementById('tabs').addEventListener("click", activeTabCapturing, false);
}, 10);


function mobileMenuClicked() {
  var sidePanel = document.getElementsByClassName("sidebar");
  var mainPanel = document.getElementsByClassName("tabbable");
  var mainPanel_component = document.getElementsByClassName("main-component");
  
  if (mainPanel[0].style.display === "none") {
    sidePanel[1].style.display = "none";
    mainPanel[0].style.display = "block";
    mainPanel_component[0].style.width = "90%"
  } else {
    sidePanel[1].style.display = "block";
    mainPanel[0].style.display = "none";
    mainPanel_component[0].style.width = "10%"
  }
}

function activeTabCapturing () {
  setTimeout(function() {
    var screenW = window.innerWidth;
    var ele = document.getElementById('tabs');
    if (screenW <= 411) {
      for(let i=0; i<ele.children.length; i++){
      var cnt = ele.children[i];
        if (cnt.classList[0] === 'active' && (cnt.innerText === "Report Preview" || cnt.innerText === "Testing Metrics" || cnt.innerText === "Upload Package") ) {
          var navtab = document.getElementsByClassName('nav-tabs')
          navtab[0].style.width = "100%"
        } else if (cnt.classList[0] === 'active' && (cnt.innerText === "Maintenance Metrics") ){
          var navtab = document.getElementsByClassName('nav-tabs');
          navtab[0].style.width = "80%"
        } else if (cnt.classList[0] === 'active' && (cnt.innerText === "Community Usage Metrics") ){
          var navtab = document.getElementsByClassName('nav-tabs');
          navtab[0].style.width = "95%"
        }
      }
    }
  }, 300); 
}

function capturingSizeOfInfoBoxes(){
  const element = document.querySelectorAll(".info-box-content");
  
  var allClHeight = [];
  for (var i = 0; i < element.length; i++) {
        allClHeight.push(element[i].clientHeight);
  }
  var mx = Math.max(...allClHeight);
  var mn = Math.min(...allClHeight);
  var diff = mx - mn;
  for (var i = 0; i < element.length; i++) {
      if (element[i].clientHeight < mx) {
          element[i].style.height = mx + 'px';
      } 
  }
}


function updateInfoBoxesWhenNA(id){
  var ele = document.getElementById(id);
  var infoBox = ele.firstChild;
  infoBox.classList.add('na-bg-color');
  
  var icn = infoBox.firstElementChild;
  icn.firstElementChild.style.display = "none";
}

function updateInfoBoxesColorWhenNA(id){
  var ele = document.getElementById(id);
  var infoBox = ele.firstChild;
  infoBox.classList.add('na-bg-color');
  
  var icn = infoBox.firstElementChild;
}

function updateText(id) {
  
  var element = document.getElementById(id).getElementsByClassName('highcharts-subtitle');
  element[0].style.fontSize = '16px';
  var xVal = element[0]['x'].baseVal[0].value;
  element[0]['y'].baseVal[0].value = xVal/2;
}

function disableUI(id) {
  
  var ele = document.getElementById(id); 
  ele.disabled = false;
  if(ele && ele !== null){
    ele.disabled = true;
  }
 }

function addTextToGaugeSVG(id) {
  
  var elementCircle = document.getElementById(id).getElementsByTagName('circle')[0];
  elementCircle.nextSibling.remove();
  elementCircle.remove();
  
  var element = document.getElementById(id).getElementsByTagName('svg')[0];
  var textElement = document.createElementNS('http://www.w3.org/2000/svg', 'text');
  textElement.setAttributeNS(null, 'x', element.width.baseVal.value/2);
  textElement.setAttributeNS(null, 'y', element.height.baseVal.value/2 +20);
  textElement.setAttributeNS(null,"font-size","20");
  textElement.setAttributeNS(null,"fill","red");
  textElement.setAttributeNS(null,"text-anchor","middle");
  textElement.setAttributeNS(null,"class","gauge-error-text");
  var txt = document.createTextNode("Metric is not applicable");
  textElement.appendChild(txt);
  element.appendChild(textElement);
  
  var textElement2 = document.createElementNS('http://www.w3.org/2000/svg', 'text');
  textElement2.setAttributeNS(null, 'x', element.width.baseVal.value/2);
  textElement2.setAttributeNS(null, 'y', element.height.baseVal.value/2 +50);
  textElement2.setAttributeNS(null,"font-size","20");
  textElement2.setAttributeNS(null,"fill","red");
  textElement2.setAttributeNS(null,"text-anchor","middle");
  textElement2.setAttributeNS(null,"class","gauge-error-text");
  var txt2 = document.createTextNode("for this source of package");
  textElement2.appendChild(txt2);
  element.appendChild(textElement2);

  //Code to replace 0 with NA
  var allEle = element.querySelectorAll('text')
  for(let i=0; i<allEle.length; i++){
    if(allEle[i].textContent == 100 && allEle[i+1].textContent == 0){
        allEle[i+1].textContent = "NA"
    }
  }
}
