/* fluff.js — nonessential quality-of-life enhancements */

function update_sidebar_or_error(req)
{
	if (req.readyState === XMLHttpRequest.DONE) {
		if (req.status !== 200) {
			document.open();
			document.write(req.responseText);
			document.close();
		} else {
			document.getElementById("sidebar").innerHTML = req.responseText;
		}
	}
}

/* This function is called when the user submits the tag editor form
 * on a datum view.  It is responsible for submitting the form on the
 * user's behalf without a page reload, and live-updating the tag
 * listing in the sidebar to reflect the changes. */
/* FIXME: A very similar function will be used for updating subtags
 * and supertags. */
function submit_tag_editor_asynchronously()
{
	const tag_data = document.getElementById("tag-editor-textarea").value;
	const req = new XMLHttpRequest();
	req.onreadystatechange = () => { update_sidebar_or_error(req); };
	req.open("POST", window.location, true);
	req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	req.send(`tags=${encodeURIComponent(tag_data)}&ajax=t`);
}

/* Submit tag editor contents asynchronously, using either the form
 * button or S-RET. */
document.getElementById("tag-editor-submit").setAttribute("type", "button");
document.getElementById("tag-editor-textarea").addEventListener("keydown", (e) => {
	if (e.shiftKey && e.keyCode == 13) submit_tag_editor_asynchronously();
});
document.getElementById("tag-editor-submit").addEventListener("click", (e) => {
	submit_tag_editor_asynchronously();
});
