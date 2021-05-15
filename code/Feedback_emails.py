import pandas as pd
import smtplib

'''
Change these to your credentials and name
'''

your_name = "Anne-Kathrin Kleine"
your_email = "a.k.kleine@rug.nl"
your_password = "m%L3L4`/ymF]Y"

# Read the file
email_list = pd.read_excel("long_pan_ID_text.csv")

# Get all the Names, Email Addreses, Subjects and Messages
all_emails = email_list['Email']
all_messages = email_list['text_var1']

# Loop through the emails
for idx in range(len(all_emails)):

    # Get each records name, email, subject and message
    message = all_messages[idx]
    subject = "Feedback Reports Entrepreneurship Study"

    # Create the email to send
    full_email = ("From: {0} <{1}>\n"
                  "To: <{2}>\n"
                  "Subject: {3}\n\n"
                  "{4}"
                  .format(your_name, your_email, email, subject, message))

    # In the email field, you can add multiple other emails if you want
    # all of them to receive the same text
    try:
        server.sendmail(your_email, [email], full_email)
        print('Email to {} successfully sent!\n\n'.format(email))
    except Exception as e:
        print('Email to {} could not be sent :( because {}\n\n'.format(email, str(e)))

# Close the smtp server
server.close()