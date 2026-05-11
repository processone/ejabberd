
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

(function () {
    'use strict';
    window.addEventListener('load', () => {
        const boxes = document.getElementsByClassName('select-all-checkbox');
        Array.prototype.forEach.call(boxes, box => {
            box.addEventListener('change', (event) => {
                event.target.closest('table').querySelectorAll('tr td input[type=checkbox]').forEach((el) => { el.checked = event.target.checked;});
            }, false);
        });
    }, false);
})();
