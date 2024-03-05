// Show/Hide an element
function sh(e) {
    if (document.getElementById(e).style.display=='none') {
	document.getElementById(e).style.display='block';
    } else {
	document.getElementById(e).style.display='none';
    }
}
// Show/Hide join/leave elements
function jlf() {
    var es = document.getElementsByClassName('jl');
    for (var i = 0; i < es.length; i++) {
        if (es[i].style.display === 'block') {
            es[i].style.display = 'none';
        } else {
            es[i].style.display = 'block';
        }
    }
}
