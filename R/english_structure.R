### Langauge data

pagetitle = "Clinical Performance Tracker"

welcome = "Welcome to the Clinical Performance Tracker"

intro1 = "The purpose of this web-app is to allow clinicians to track performance on treatment targets. In single-case design research studies, participants are typically asked to complete brief assessments of their performance on treatment targets at regular intervals. Performance on these assessments allows researchers to track changes and estimate effect sizes."

intro2 = "However, implementing the same type of performance tracking can be more challenging in clinical practice. First, it is rarely appropriate to utilize multiple 'baseline' assessments before treatment starts, as treatment time is limited and experimental control is less important. But the lack of baseline assessments means that most single-case design statistical approaches and effect sizes are not applicable. Second, treatment targets often change over time or it may not be practical to include all treated items in every assessment, which makes it hard to track progress on performance in general. More complex statistical methods can help to resolve some of these challenges, but clinicians may not have the time or experience to implement them."

intro3 = "This web-app allows clinicians to easily create a simple assessment program, score performance, and download the results. Clinicians can upload many image files (note, they can be images of anything: words, sentences, single pictures, personally relevant pictures, complex scenes...). The web-app will create a simple assessment task using the images. Items are scored as correct or incorrect during assessment by the clinician. The accuracy of a single session is calculated and can be estimated statistically. Clinicians can download a spreadsheet (.csv) file with their client's performance. At the next assessment, the clinicians simply needs to re-upload the same (or different!) images and complete the test again. The previous .csv file is uploaded, and the results are combined. After 2 assessments, treatment effect sizes can be estimated statistically (though at least 4-5 assessments are recommended at minimum)."

instructions1 = "1. Enter a name (or initials, or pseudonym) and ensure the date is correct.*"

instructions2 =  "2. Upload images for the brief assessment. Ideally, at least 10 items are used. Allowable formats are .jpeg, .jpg, .png.* Images can contain any kind of stimuli where a correct response can be scored relatively quickly by the clinician. There are some sample images already uploaded to test out the application. To use these, simply skip the image upload."

instructions3 =  "3. Select whether to randomize the order of images (recommended, unless a specific order is desired). Click start assessment."

instructions4 =  "4. During the assessment, performance is scored by keypress. 1 for incorrect. 2 for correct. The screen is advanced by hitting 'enter'. Feedback is not shown to participants during the assessment - whether or not to give feedback is left up to the clinician."

instructions5 = "5. After the last image, accuracy is shown and performance on each item is presented in a table. A simple statistical model can run to provide an estimate of overall accuracy and a level of uncertainty around that estimate."

instructions6 = "6. If it's the first session, navigate to the download page to download the data. That's it!"

instructions7 = "7. If it is not the first session, click on the 'Effect Sizes' button and upload the files from the previous session. After uploading prior data, effect sizes can be calculated. These include the rate of improvement and the total number of estimated items improved. These effect sizes come with credible intervals, so you can tell if improvement is statistically different than zero."

note1 = "*This app does not save any data entered by users after the browser window is closed. The name field is only included on the downloadable spreadsheet. This can be verified in the source code, which is openly available. Still, patient privacy is important and risks should be minimized where possible."

faq1 = "Q: What kind of items or stimuli can I use?  A: Any kind of stimuli where there is a relatively clear correct/incorrect response and you can include at least 10 items. Consider how the stimuli are connected to treatment. Are you doing VNeST for a set of 15 personally relevant verbs? Then there's your stimuli set! Also consider that this assessment should be quick, and not take away from more important uses of clinical time"

faq2 = "Q: How can I create lots of images like this? A: An easy way to create lots of images files for stimuli is to make powerpoint slides (or using google docs). Slides can be saved from powerpoint/google docs as individual image files. Just be cautious of file size - upload sizes are currently limited."

faq3 = "Q: So I have to keep track of the images and performance files? A: Yup - I recommend just keeping a folder for each client. Inside, keep a sub-folder of the current images and the current spreadsheet"

faq4 = "Q: Is it mobile friendly, or what about Ipad? A: The app may work on these devices, but it is not currently designed to do so."

faq5 = "Q: What about remote/online administration? A: As long as you can share your screen, there's no reason this can't be  administered online."

faq6 = "Q: What if I want to change or update stimuli? A: The statistical models used to calculate effect sizes don't need the same items to be adminstered every time. For example, if you had too many items, you could assess half each time, and the effect sizes would be estimated based on all items. However, adding new items to the stimuli set throughout the treatment period may make the effect sizes seem smaller than they actually are."

faq7 = "Q: What if I want to include some words I'm not working on to test generalization? A: The current effect size approach is agnostic to whether or not a word is treated (perhaps a future feature)."


nameinput = "Enter a Name"

otherinput = "Enter any other notes"

dateinput = "Select date and time"



inputstart = "Start Assessment"

tabtitle0 = "Home"
tabtitle1 = "Assessment"
tabtitle2 = "Results"
tabtitle3 = "Progress"


backbutton = "Back"

nextbutton = "Next"

resultstext = "This page has the results"

citation = div(
  
  h3("References"),
  
  p("Dell, G. S., Schwartz, M. F., Martin, N., Saffran, E. M., & Gagnon, D. A. (1997). Lexical access in aphasic and nonaphasic speakers. Psychological Review, 104, 801-838.PMID 9337631"),
  
  p("Roach, A., Schwartz, M.F., Martin, N., Grewal, R.S., & Brecher, A. (1996). The Philadelphia Naming Test: Scoring and Rationale. Clinical Aphasiology, 24, 121-133."),
  
    p("Schwartz, M.F., Dell, G.S., Martin, N., Gahl, S., & Sobel, P. ( 2006). A case-series test of the interactive two-step model of lexical access: Evidence from picture naming. Journal of Memory and Language, 54, 228-264.")
)

