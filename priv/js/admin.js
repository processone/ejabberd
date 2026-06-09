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
