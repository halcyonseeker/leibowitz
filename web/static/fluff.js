/* fluff.js — nonessential quality-of-life enhancements */

function update_sidebar_or_error(req)
{
	if (req.readyState === XMLHttpRequest.DONE) {
		if (req.status !== 200) {
			/* Hopefully the backend sent an error page */
			document.open();
			document.write(req.responseText);
			document.close();
		} else {
			document.getElementById("sidebar").innerHTML = req.responseText;
		}
	}
}

function submit_tag_editor_asynchronously()
{
	const tag_data = document.getElementById("tag-editor-textarea").value;
	const req = new XMLHttpRequest();
	/* Register a response handler */
	req.onreadystatechange = () => { update_sidebar_or_error(req); };
	req.open("POST", window.location, true);
	req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	/* Specifying ajax=t tells the endpoint handler to return a
	 * snippet of HTML for the sidebar rather than redirecting */
	req.send(`tags=${encodeURIComponent(tag_data)}&ajax=t`);
}

/* Suppress default submit button behavior */
document.getElementById("tag-editor-submit").setAttribute("type", "button");
/* Handle S-RET; FIXME: avoid inserting a newline */
document.getElementById("tag-editor-textarea").addEventListener("keydown", (e) => {
	if (e.shiftKey && e.keyCode == 13) submit_tag_editor_asynchronously();
});
/* Handle press of the submit button asynchronously */
document.getElementById("tag-editor-submit").addEventListener("click", (e) => {
	submit_tag_editor_asynchronously();
});
