/* fluff.js — nonessential quality-of-life enhancements
 *
 * NOTES
 * =====
 * * https://news.ycombinator.com/hn.js
 * * Some bad advice, some good https://phuoc.ng/collection/html-dom/
 *   * Discussion: https://news.ycombinator.com/item?id=38162435
 * * How to no jquery https://youmightnotneedjquery.com/
 * * How to vanilla js https://javascript.info/
 */

/***********************************************************************
 * Helper Functions */

function $(id) { return document.getElementById(id); }

function replace_page_with(text)
{
	console.log("DBG: replace_page_with TEXT of type " + typeof(text));
	document.open();
	document.write(text);
	document.close();
}

/***********************************************************************
 * AJAX Form Submissions */

function update_sidebar_or_error(req)
{
	if (req.readyState === XMLHttpRequest.DONE) {
		if (req.status !== 200)
			/* Hopefully the backend sent an error page,
			 * FIXME: add some frontend error handling
			 * just in case it didn't */
			replace_page_with(req.responseText);
		else
			$("sidebar").innerHTML = req.responseText;
	}
}

function submit_tag_editor_asynchronously()
{
	const tag_data = $("tag-editor-textarea").value;
	const req = new XMLHttpRequest();
	/* Register a response handler */
	req.onreadystatechange = () => { update_sidebar_or_error(req); };
	req.open("POST", window.location, true);
	req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	/* Specifying ajax=t tells the endpoint handler to return a
	 * snippet of HTML for the sidebar rather than redirecting */
	req.send(`tags=${encodeURIComponent(tag_data)}&ajax=t`);
}

if ($("tag-editor-submit")) {
	/* Suppress default submit button behavior */
	$("tag-editor-submit").setAttribute("type", "button");
	/* Handle press of the submit button asynchronously */
	$("tag-editor-submit").addEventListener("click", (e) => {
		submit_tag_editor_asynchronously();
	});
}

if ($("tag-editor-textarea")) {
	/* Handle S-RET; FIXME: avoid inserting a newline */
	$("tag-editor-textarea").addEventListener("keydown", (e) => {
		if (e.shiftKey && e.keyCode == 13)
			submit_tag_editor_asynchronously();
	});
}
