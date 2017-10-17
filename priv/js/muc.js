// Show/Hide an element
function sh(e) {
    if (document.getElementById(e).style.display=='none') {
	document.getElementById(e).style.display='block';
    } else {
	document.getElementById(e).style.display='none';
    }
}
