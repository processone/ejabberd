
function selectAll() {
  for(i=0;i<document.forms[0].elements.length;i++)
  { var e = document.forms[0].elements[i];
    if(e.type == 'checkbox')
    { e.checked = true; }
  }
}
function unSelectAll() {
  for(i=0;i<document.forms[0].elements.length;i++)
  { var e = document.forms[0].elements[i];
    if(e.type == 'checkbox')
    { e.checked = false; }
  }
}
