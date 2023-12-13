# Markdown to HTML Converter with Simple GUI

To run the converter:

- stack build
- stack run
- Copy the source Markdown into the top text area and hit enter.
- If the Markdown is correctly formatted, the new HTML will appear in the bottom text area.

For more information on Monomer and for project dependencies, check https://github.com/fjvallarino/monomer.
For more information on basic Markdown syntax, check https://www.markdownguide.org/basic-syntax/.

Some important syntax notes:

- The GUI will convert the entered text with the "enter" key, so use shift + enter to get a newline in the GUI.
- No space between list and header identifiers and the content text.
- Space between the asterisks and content text for bold selections.
- When making an element bold and italic, use two asterisks and an underline \*\*\_ \_\*\*

Important Note : This program is meant to be used to generate basic HTML elements from their corresponding Markdown
elements. Entire document converting is unstable at best, and generates parse errors at worst.
