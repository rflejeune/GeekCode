       identification division.
       program-id. geekcode.
      ******************************************************************
      *                                                                *
      *Author. Randy LeJeune.                                          *
      *Date-written.  29  Sep 2010. (v. 0.1)                           *
      *Edited:		  22 April 2013   - Minor Typos fixed.             *
      *                               - Code converted to lowercase    *
      *                                 (v. 0.2)                       *
      *               18 October 2013 - Spelling Errors & Typos        *
      *                                 corrected. (v. 0.3)            *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *   This program is free software; you can redistribute it       *
      *   and/or modify it under the terms of the GNU General Public   *
      *   License as published by the Free Software Foundation; either *
      *   version 2 of the License, or at your option) any later       *
      *   version.                                                     *
      *                                                                *
      *   This program is distributed in the hope that it will be      *
      *   useful, but WITHOUT ANY WARRANTY; without even the implied   *
      *   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      *
      *   PURPOSE.  See the GNU General Public License for more        *
      *   details.                                                     *
      *                                                                *
      *   You should have received a copy of the GNU General Public    *
      *   License along with this program; if not, write to the Free   *
      *   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,*
      *                                                                *
      *   Interface Design taken from Chris Gushue's geekcode generator*
      *                                                                *
      ******************************************************************
      ******************************************************************

       environment division.

       configuration section.

           source-computer. ibm-686.
           object-computer. ibm-686.

       input-output section.

       file-control.
           select geek-sig             assign to "geekcode.sig"
                                       organization is line sequential.
       data division.

       file section.

       fd  geek-sig. 
       01  geek-output-rec             pic x(80).

       working-storage section.

       copy "geekcode.cpy".

       77  ws-print-line1              pic x(80)     value spaces.
       77  ws-print-line2              pic x(80)     value spaces.
       77  ws-print-line3              pic x(80)     value spaces.
       77  ws-print-line4              pic x(80)     value spaces.

       77  ws-command                  pic a(20)     value spaces.
       77  ws-cl-args                  pic x(10)     value spaces.
       77  ws-page-cnt                 pic 99        value zeroes.
       77  ws-cnt                      pic xx        value spaces.
       77  ws-tot-page                 pic 99        value 45.
       77  ws-rec-cnt                  pic 9         value zeroes.
       77  ws-return-sys-code          pic 9(8) comp value zeroes.
       77  ws-entry                    pic xx        value zeroes.

       01 ws-valid-flag                pic x         value "n".
          88  ws-valid-data                          value "y".
          88  ws-invalid-data                        value "n".

       01 ws-valid-pens                pic x         value "n".
          88  ws-how-many                            value "y".

       procedure division.

       00000-control.
           perform 10000-setup
           perform 20000-process
           perform 30000-cleanup.

       10000-setup.
           accept ws-cl-args from command-line end-accept
           perform 93000-parse-cmdln
           open output geek-sig
           initialize  geek-output-rec.

       20000-process.
           perform 90000-clear-screen
           perform 91000-print-heading
           perform 21000-create.

       21000-create.
           perform 22100-type      until ws-valid-data
           set ws-valid-flag to "n"
           perform 22200-dress     until ws-valid-data
           set ws-valid-flag to "n"
           perform 22300-hair      until ws-valid-data
           set ws-valid-flag to "n"
           perform 22400-height    until ws-valid-data
           set ws-valid-flag to "n"
           perform 22450-weight    until ws-valid-data
           set ws-valid-flag to "n"
           perform 22500-glasses   until ws-valid-data
           set ws-valid-flag to "n"
           perform 22600-pens      until ws-valid-data
           set ws-valid-flag to "n"
           perform 22670-slides    until ws-valid-data
           set ws-valid-flag to "n"
           perform 22700-auto      until ws-valid-data
           set ws-valid-flag to "n"
           perform 22800-age       until ws-valid-data
           set ws-valid-flag to "n"
           perform 22900-weird     until ws-valid-data
           set ws-valid-flag to "n"
           perform 23000-verbage   until ws-valid-data
           set ws-valid-flag to "n"
           perform 23100-comp      until ws-valid-data
           set ws-valid-flag to "n"
           perform 23200-flavor    until ws-valid-data
           set ws-valid-flag to "n"
           perform 23250-unix      until ws-valid-data
           set ws-valid-flag to "n"
           perform 23300-perl      until ws-valid-data
           set ws-valid-flag to "n"
           perform 23400-linux     until ws-valid-data
           set ws-valid-flag to "n"
           perform 23500-386bsd    until ws-valid-data
           set ws-valid-flag to "n"
           perform 23600-news      until ws-valid-data
           set ws-valid-flag to "n"
           perform 23700-web       until ws-valid-data
           set ws-valid-flag to "n"
           perform 23800-emacs     until ws-valid-data
           set ws-valid-flag to "n"
           perform 23900-kibo      until ws-valid-data
           set ws-valid-flag to "n"
           perform 24000-ms        until ws-valid-data
           set ws-valid-flag to "n"
           perform 24100-mac       until ws-valid-data
           set ws-valid-flag to "n"
           perform 24200-vms       until ws-valid-data
           set ws-valid-flag to "n"
           perform 24400-pol       until ws-valid-data
           set ws-valid-flag to "n"
           perform 24500-cp        until ws-valid-data
           set ws-valid-flag to "n"
           perform 24700-trek      until ws-valid-data
           set ws-valid-flag to "n"
           perform 24800-bab       until ws-valid-data
           set ws-valid-flag to "n"
           perform 25000-jeop      until ws-valid-data
           set ws-valid-flag to "n"
           perform 25100-role      until ws-valid-data
           set ws-valid-flag to "n"
           perform 25200-magic     until ws-valid-data
           set ws-valid-flag to "n"
           perform 25300-tv        until ws-valid-data
           set ws-valid-flag to "n"
           perform 25400-books     until ws-valid-data
           set ws-valid-flag to "n"
           perform 25500-doom      until ws-valid-data
           set ws-valid-flag to "n"
           perform 25600-barney    until ws-valid-data
           set ws-valid-flag to "n"
           perform 25700-educ      until ws-valid-data
           set ws-valid-flag to "n"
           perform 25800-music     until ws-valid-data
           set ws-valid-flag to "n"
           perform 25900-house     until ws-valid-data
           set ws-valid-flag to "n"
           perform 26000-friends   until ws-valid-data
           set ws-valid-flag to "n"
           perform 26100-rel       until ws-valid-data
           set ws-valid-flag to "n"
           perform 26200-nut       until ws-valid-data
           set ws-valid-flag to "n"
           perform 26300-gender    until ws-valid-data
           set ws-valid-flag to "n"
           perform 26350-sex       until ws-valid-data
           perform 26500-print.

       22100-type.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Geek Type
      -       "             Page: " ws-cnt " of " ws-tot-page
                  end-display
           display "====================================================
      -"==========================="
               end-display
           display " 1 GB  - Geek of Business                15 GL  -
      -"Geek of Literature"
               end-display
           display " 2 GC  - Geek of Classics                16 GMC -
      -"Geek of Mass Communications"
               end-display
           display " 3 GCA - Geek of Commercial Arts         17 GM  -
      -"Geek of Math"
               end-display
           display " 4 GCM - Geek of Computer Management     18 GMD -
      -"Geek of Medicine"
               end-display
           display " 5 GCS - Geek of Computer Science        19 GMU -
      -"Geek of Music"
               end-display
           display " 6 GCC - Geek of Communications          20 GPA -
      -"Geek of Performing Arts"
               end-display
           display " 7 GE  - Geek of Engineering             21 GP  -
      -"Geek of Philosophy"
               end-display
           display " 8 GED - Geek of Education               22 GS  -
      -"Geek of Science"
               end-display
           display " 9 GFA - Geek of Fine Arts               23 GSS -
      -"Geek of Social Science"
               end-display
           display "10 GG  - Geek of Government              24 GTW -
      -"Geek of Technicial Writing"
               end-display
           display "11 GH  - Geek of Humanities              25 GO  -
      -"Geek of Other"
               end-display
           display "12 GIT - Geek of Information Technology  26 GU  -
      -"Geek of Undecided"
               end-display
           display "13 GJ  - Geek of Jurisprudence (Law)     27 G!  -
      -"Geek of No Qualifications"
               end-display
           display "14 GLS - Geek of Library Science         28 GAT -
      -"Geek of All Trades"
               end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Geek Type code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 28
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "GL" to ws-type
                   else if ws-entry = 02 then
                       move "GC" to ws-type
                   else if ws-entry = 03 then
                       move "GCA" to ws-type
                   else if ws-entry = 04 then
                       move "GCM" to ws-type
                   else if ws-entry = 05 then
                       move "GCS" to ws-type
                   else if ws-entry = 06 then
                       move "GCC" to ws-type
                   else if ws-entry = 07 then
                       move "GE" to ws-type
                   else if ws-entry = 08 then
                       move "GED" to ws-type
                   else if ws-entry = 09 then
                       move "GFA" to ws-type
                   else if ws-entry = 10 then
                       move "GG" to ws-type
                   else if ws-entry = 11 then
                       move "GH" to ws-type
                   else if ws-entry = 12 then
                       move "GIT" to ws-type
                   else if ws-entry = 13 then
                       move "GJ" to ws-type
                   else if ws-entry = 14 then
                       move "GLS" to ws-type
                   else if ws-entry = 15 then
                       move "GL" to ws-type
                   else if ws-entry = 16 then
                       move "GMC" to ws-type
                   else if ws-entry = 17 then
                       move "GM" to ws-type
                   else if ws-entry = 18 then
                       move "GMD" to ws-type
                   else if ws-entry = 19 then
                       move "GMU" to ws-type
                   else if ws-entry = 20 then
                       move "GPA" to ws-type
                   else if ws-entry = 21 then
                       move "GP" to ws-type
                   else if ws-entry = 22 then
                       move "GS" to ws-type
                   else if ws-entry = 23 then
                       move "GSS" to ws-type
                   else if ws-entry = 24 then
                       move "GTW" to ws-type
                   else if ws-entry = 25 then
                       move "GO" to ws-type
                   else if ws-entry = 26 then
                       move "GU" to ws-type
                   else if ws-entry = 27 then
                       move "G!" to ws-type
                   else if ws-entry = 28 then
                       move "GAT" to ws-type
                   end-if 
               end-if
           end-if.

           perform 90000-clear-screen.

       22200-dress.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Dress                                               
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1  d++  I tend to wear conservative dress such as "
              "a business suit or "
           end-display
           display "         worse, a tie." end-display
           display " 2  d+   Good leisure-wear. Slacks, button-shirt, "
               "etc. No jeans, tennis "
           end-display
           display "         shoes, or t-shirts." end-display
           display " 3  d    I dress a lot like those found in catalog "
               "ads. Bland, boring, "
           end-display
           display "         without life or meaning." end-display
           display " 4  d-   I'm usually in jeans and a t-shirt."
           end-display
           display " 5  d--  My t-shirts go a step further and have a "
               "trendy political "
           end-display
           display "         message on them." end-display
           display " 6  d--- Punk dresser, including, but not limited "
               "to, torn jeans and "
           end-display
           display "         shirts, body piercings, and prominent "
               "tattoos."
           end-display
           display " 7  dx   Cross dresser." end-display
           display " 8  d?   I have no idea what I am wearing now, "
               "let alone what I wore yesterday."
           end-display
           display " 9  !d   No clothing. Quite a fashion statement, "
               "don't you think?"
           end-display
           display "10  dpu  I wear the same clothes all the time, no "
               "matter the occasion, "
           end-display
           display "         forgetting to do laundry between wearings."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Dress code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "d++" to ws-dress
                   else if ws-entry = 02 then
                       move "d+" to ws-dress
                   else if ws-entry = 03 then
                       move "d" to ws-dress
                   else if ws-entry = 04 then
                       move "d-" to ws-dress
                   else if ws-entry = 05 then
                       move "d--" to ws-dress
                   else if ws-entry = 06 then
                       move "d---" to ws-dress
                   else if ws-entry = 07 then
                       move "dx" to ws-dress
                   else if ws-entry = 08 then
                       move "d?" to ws-dress
                   else if ws-entry = 09 then
                       move "!d" to ws-dress
                   else if ws-entry = 10 then
                       move "dpu" to ws-dress
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22300-hair.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Hair                                                
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 H+++   My hair goes down past my waist."
           end-display
           display " 2 H++    My hair dangles to my mid-back."
           end-display
           display " 3 H+     It's down to about my shoulders."
           end-display
           display " 4 H      It's just pretty normal hair."
           end-display
           display " 5 H-     It's cut above the neck."
           end-display
           display " 6 H--    Above the neck AND ear (flattop)."
           end-display
           display " 7 H---   It's about 1/8 inch long."
           end-display
           display " 8 H----  I shave my head daily, otherwise it gets "
               "too long."
           end-display
           display " 9 !H     I'm bald."
           end-display
           display "10 H?     I have wigs that allow me to vary my "
               "hair."
           end-display
           display "11 H*     My hair is dyed funky flavors."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Hair code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 11
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "H+++" to ws-hair
                   else if ws-entry = 02 then
                       move "H++" to ws-hair
                   else if ws-entry = 03 then
                       move "H+" to ws-hair
                   else if ws-entry = 04 then
                       move "H" to ws-hair
                   else if ws-entry = 05 then
                       move "H-" to ws-hair
                   else if ws-entry = 06 then
                       move "H--" to ws-hair
                   else if ws-entry = 07 then
                       move "H---" to ws-hair
                   else if ws-entry = 08 then
                       move "H----" to ws-hair
                   else if ws-entry = 09 then
                       move "!H" to ws-hair
                   else if ws-entry = 10 then
                       move "H?" to ws-hair
                   else if ws-entry = 11 then
                       move "H*" to ws-hair
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22400-height.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Height                                              
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 s+++   I usually have to duck through doors. "
           end-display
           display " 2 s++    I'm a basketball candidate. "
           end-display
           display " 3 s+     I'm a little taller than most. "
           end-display
           display " 4 s      I'm an average geek. "
           end-display
           display " 5 s-     I look up to most people. "
           end-display
           display " 6 s--    I look up to damn near everybody. "
           end-display
           display " 7 s---   I take a phone book with me when I go "
           end-display
           display "          out so I can eat dinner. "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Height code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "s+++" to ws-height
                   else if ws-entry = 02 then
                       move "s++" to ws-height
                   else if ws-entry = 03 then
                       move "s+" to ws-height
                   else if ws-entry = 04 then
                       move "s" to ws-height
                   else if ws-entry = 05 then
                       move "s-" to ws-height
                   else if ws-entry = 06 then
                       move "s--" to ws-height
                   else if ws-entry = 07 then
                       move "s---" to ws-height
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22450-weight.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Weight                                              
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 s+++   I take up three movie seats. "
           end-display
           display " 2 s++    I'm a linebacker candidate. "
           end-display
           display " 3 s+     I'm a little rounder than most."
           end-display
           display " 4 s      I'm an average geek."
           end-display
           display " 5 s-     Everybody tells me to gain a few pounds."
           end-display
           display " 6 s--    I tend to have to fight against a strong "
               "breeze."
           end-display
           display " 7 s---   My bones are poking through my skin. "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Weight code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "+++" to ws-weight
                   else if ws-entry = 02 then
                       move "++" to ws-weight
                   else if ws-entry = 03 then
                       move "+" to ws-weight
                   else if ws-entry = 04 then
                       move " " to ws-weight
                   else if ws-entry = 05 then
                       move "-" to ws-weight
                   else if ws-entry = 06 then
                       move "--" to ws-weight
                   else if ws-entry = 07 then
                       move "---" to ws-weight
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22500-glasses.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Glasses                                             
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 g+++   I have coke-bottle glasses that I can "
               "use to start leaves on "
           end-display
           display "          fire in the hot sun."
           end-display
           display " 2 g++    I've got four eyes and tape in the "
               "middle.  "
           end-display
           display " 3 g+     I've got four eyes, what's your point?"
           end-display
           display " 4 g-     I have contacts."
           end-display
           display " 5 g--    I have colored contacts I have contacts."
           end-display
           display " 6 g---   I have those funky contact that have "
              "interesting designs on"
           end-display
           display "          then such as happy faces or some such. "
           end-display
           display " 7 !g     I have no glasses."
           end-display
           display " 8 g?     I can't find my glasses."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Glasses code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "g+++" to ws-glasses
                   else if ws-entry = 02 then
                       move "g++" to ws-glasses
                   else if ws-entry = 03 then
                       move "g+" to ws-glasses
                   else if ws-entry = 04 then
                       move "g-" to ws-glasses
                   else if ws-entry = 05 then
                       move "g--" to ws-glasses
                   else if ws-entry = 06 then
                       move "g---" to ws-glasses
                   else if ws-entry = 07 then
                       move "!g" to ws-glasses
                   else if ws-entry = 08 then
                       move "g?" to ws-glasses
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22600-pens.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Pens                                          
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display "Do you have any pens in your pockets? "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display " 1 Yes." end-display
           display " 2 No." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Pens code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 02
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "p" to ws-pens (1:1)
                       perform 90000-clear-screen
                       perform 22650-how-many until ws-how-many
                   else if ws-entry = 02 then
                       perform 90000-clear-screen
                       perform 22660-nopens
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22650-how-many.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Pens                                          
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display "How many pens do you have in your pockets? "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display " 1 One." end-display
           display " 2 Two." end-display
           display " 3 Three." end-display
           display " 4 Four." end-display
           display " 5 Five." end-display
           display " 6 Six." end-display
           display " 7 Seven." end-display
           display " 8 Eight." end-display
           display " 9 Nine." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Pens Number code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-pens to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move 1 to ws-pens (2:1)
                   else if ws-entry = 02 then
                       move 2 to ws-pens (2:1)
                   else if ws-entry = 03 then
                       move 3 to ws-pens (2:1)
                   else if ws-entry = 04 then
                       move 4 to ws-pens (2:1)
                   else if ws-entry = 05 then
                       move 5 to ws-pens (2:1)
                   else if ws-entry = 06 then
                       move 6 to ws-pens (2:1)
                   else if ws-entry = 07 then
                       move 7 to ws-pens (2:1)
                   else if ws-entry = 08 then
                       move 8 to ws-pens (2:1)
                   else if ws-entry = 09 then
                       move 9 to ws-pens (2:1)
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22660-nopens.
           exit.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Pens                                          
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 p?     I can't find a writing instrument."
           end-display
           display " 2 !p     Pens are obsolete. I have a Newton."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your No Pens code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 02
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "?" to ws-pens (2:1)
                   else if ws-entry = 02 then
                       move "!p" to ws-pens
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22670-slides.
           add 1 to ws-page-cnt
           move ws-page-cnt (2:1) to ws-cnt

           display "Slide Rules, Etc.                                   
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display "Do you carry a slide rule, calculator or portable co
      -        "mputer along with you?"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display " 1 Yes." end-display
           display " 2 No." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Extra Stuff code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 02
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "+" to ws-pens (3:1)
                   else if ws-entry = 02 then
                       continue
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22700-auto.
           add 1 to ws-page-cnt
           move ws-page-cnt to ws-cnt

           display "Automobile                                          
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 au++++ I have my chauffeured limo take me "
               "everywhere."
           end-display
           display " 2 au+++  I own four different colored Mercedes."
           end-display
           display " 3 au++   I drive a brand new car that cost more "
               "than most houses"
           end-display
           display " 4 au+    I have a sporty-looking car which would "
               "be a babe-mobile if"
           end-display
           display "          I wasn't such a geek."
           end-display
           display " 5 au     I drive a car which I bought from my "
               " parents. It has four doors even "
           end-display
           display "          though I'm the only one who ever rides in 
      -        "it. "
           end-display
           display " 6 au-    I drive my parents' car. Hey, if I could "
               "afford my own I wouldn't "
           end-display
           display "          be living at home with them. "
           end-display
           display " 7 au--   My car has rust everywhere and the "
              "muffler drags along the ground.  "
           end-display
           display " 8 au---  I drive a '77 Pinto which went over "
               "100,000 miles two years ago.  "
           end-display
           display " 9 au---- I have a Yugo."
           end-display
           display "10 !au    I don't have a car."
           end-display
           display "11 au*    I have a motorcycle."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Automobile code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 11
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "au++++" to ws-auto
                   else if ws-entry = 02 then
                       move "au+++" to ws-auto
                   else if ws-entry = 03 then
                       move "au++" to ws-auto
                   else if ws-entry = 04 then
                       move "au+" to ws-auto
                   else if ws-entry = 05 then
                       move "au" to ws-auto
                   else if ws-entry = 06 then
                       move "au-" to ws-auto
                   else if ws-entry = 07 then
                       move "au--" to ws-auto
                   else if ws-entry = 08 then
                       move "au---" to ws-auto
                   else if ws-entry = 09 then
                       move "au----" to ws-auto
                   else if ws-entry = 10 then
                       move "!au" to ws-auto
                   else if ws-entry = 11 then
                       move "au*" to ws-auto
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22800-age.
           add 1 to ws-page-cnt
           move ws-page-cnt to ws-cnt

           display "Age                                                 
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 a+++   60 and up." end-display
           display " 2 a++    50-59." end-display
           display " 3 a+     40-49." end-display
           display " 4 a      30-39." end-display
           display " 5 a-     20-29." end-display
           display " 6 a--    10-19." end-display
           display " 7 a---   9 and under." end-display
           display " 8 a?     Ageless." end-display
           display " 9 !a     It's none of your business how old I am."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Age code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                               move "a+++" to ws-age
                   else if ws-entry = 02 then
                       move "a++" to ws-age
                   else if ws-entry = 03 then
                       move "a+" to ws-age
                   else if ws-entry = 04 then
                       move "a" to ws-age
                   else if ws-entry = 05 then
                       move "a-" to ws-age
                   else if ws-entry = 06 then
                       move "a--" to ws-age
                   else if ws-entry = 07 then
                       move "a---" to ws-age
                   else if ws-entry = 08 then
                       move "a?" to ws-age
                   else if ws-entry = 09 then
                       move "!a" to ws-age
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       22900-weird.
           add 1 to ws-page-cnt
           move ws-page-cnt to ws-cnt

           display "Weirdness                                           
      -       "             Page: " ws-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 w+++  Mainstream? I heard of that once, I think."
           end-display
           display " 2 w++   I am so weird, I make Al Yankovic look sane
      -        ".  "
           end-display
           display " 3 w+    So? What's your problem with weird."
           end-display
           display " 4 w     I am not weird. I'm perfectly normal."
           end-display
           display " 5 w-    I'm more normal that most people normally a
      -        "re."
           end-display
           display " 6 w--   I am so incredibly boring . . .  "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Weirdness code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 06
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "w+++" to ws-weird
                   else if ws-entry = 02 then
                       move "w++" to ws-weird
                   else if ws-entry = 03 then
                       move "w+" to ws-weird
                   else if ws-entry = 04 then
                       move "w" to ws-weird
                   else if ws-entry = 05 then
                       move "w-" to ws-weird
                   else if ws-entry = 06 then
                       move "w--" to ws-weird
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23000-verbage.
           add 1 to ws-page-cnt

           display "Verbage                                             
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 v---   I don't talk. I just type. "
           end-display
           display " 2 v--    When I talk, people usually look mildly em
      -        "barrassed. "
           end-display
           display " 3 v-     I use words like 'grok' in everyday conver
      -        "sation."
           end-display
           display " 4 v      At least I speak in complete sentences. Us
      -        "ually. "
           end-display
           display " 5 v+     People compliment me on my vocabulary. "
           end-display
           display " 6 v++    People compliment me on my eloquence. "
           end-display
           display " 7 v+++   I was the regional forensics champ. "
           end-display
           display " 8 !v     Speech is irrelevant, I use telepathy. "
           end-display
           display " 9 v?     I mumble.  " end-display
           display "10 v*     I babble.  " end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Verbage code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "v---" to ws-verbage
                   else if ws-entry = 02 then
                       move "v--" to ws-verbage
                   else if ws-entry = 03 then
                       move "v-" to ws-verbage
                   else if ws-entry = 04 then
                       move "v" to ws-verbage
                   else if ws-entry = 05 then
                       move "v+" to ws-verbage
                   else if ws-entry = 06 then
                       move "v++" to ws-verbage
                   else if ws-entry = 07 then
                       move "v+++" to ws-verbage
                   else if ws-entry = 08 then
                       move "!v" to ws-verbage
                   else if ws-entry = 09 then
                       move "v?" to ws-verbage
                   else if ws-entry = 10 then
                       move "v*" to ws-verbage
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23100-comp.
           add 1 to ws-page-cnt

           display "Computers                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 C++++  I'll be first in line to get the new cyber
      -       "netic interface installed "
           end-display
           display "          into my skull. " end-display
           display " 2 C+++   You mean there is life outside of Internet
      -        "? You're shittin' me! I "
           end-display
           display "          live for muds. I haven't dragged myself to
      -        " class in weeks. "
           end-display
           display " 3 C++    Computers are a large part of my existenc
      -        "ence. When I get up in the "
           end-display
           display "          morning, the first thing I do is log myse
      -        "lf in. I mud on weekends, "
           end-display
           display "          but still manage to stay off of academic p
      -        "robation."
           end-display
           display " 4 C+     Computers are fun and I enjoy using them.
      -        "I play a mean game of DOOM! "
           end-display
           display "          and can use a word processor without resor
      -        "ting to the manual too. "
           end-display
           display "          often. I know that a 3.5 inch disk is not
      -        " a hard disk. I also "
           end-display
           display "          know that when it says 'press any key' to 
      -        "continue, I don't have to "
           end-display
           display "          look for a key labeled 'ANY'.  "
           end-display
           display " 5 C      Computers are a tool, nothing more. I use 
      -        "it when it serves my "
           end-display
           display "          purpose." end-display
           display " 6 C-     Anything more complicated than my calculat
      -        "or and I'm screwed. "
           end-display
           display " 7 C--    Where's the on switch? " end-display
           display " 8 C---   If you even mention computers, I will rip
      -         "your head off!  "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Computer code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "C++++" to ws-comp
                   else if ws-entry = 02 then
                       move "C+++" to ws-comp
                   else if ws-entry = 03 then
                       move "C++" to ws-comp
                   else if ws-entry = 04 then
                       move "C+" to ws-comp
                   else if ws-entry = 05 then
                       move "C" to ws-comp
                   else if ws-entry = 06 then
                       move "C-" to ws-comp
                   else if ws-entry = 07 then
                       move "C--" to ws-comp
                   else if ws-entry = 08 then
                       move "C---" to ws-comp
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23200-flavor.
           add 1 to ws-page-cnt

           display "UNIX Flavor                                         
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 B  BSD (use this unless your BSDish system is men
      -        "tioned below)." end-display
           display " 2 L  Linux" end-display
           display " 3 U  Ultrix" end-display
           display " 4 A  AIX" end-display
           display " 5 V  SysV" end-display
           display " 6 H  HP-UX" end-display
           display " 7 I  IRIX" end-display
           display " 8 O  OSF/1" end-display
           display " 9 S  SunOS / Solaris" end-display
           display "10 C  SCO UNIX" end-display
           display "11 N  NeXT" end-display
           display "12 ?  Some other one not listed." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your UNIX Flavor code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 12
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "UB" to ws-unix-flavor
                   else if ws-entry = 02 then
                       move "UL" to ws-unix-flavor
                   else if ws-entry = 03 then
                       move "UU" to ws-unix-flavor
                   else if ws-entry = 04 then
                       move "UA" to ws-unix-flavor
                   else if ws-entry = 05 then
                       move "UV" to ws-unix-flavor
                   else if ws-entry = 06 then
                       move "UH" to ws-unix-flavor
                   else if ws-entry = 07 then
                       move "UI" to ws-unix-flavor
                   else if ws-entry = 08 then
                       move "UO" to ws-unix-flavor
                   else if ws-entry = 09 then
                       move "US" to ws-unix-flavor
                   else if ws-entry = 10 then
                       move "UC" to ws-unix-flavor
                   else if ws-entry = 11 then
                       move "UN" to ws-unix-flavor
                   else if ws-entry = 12 then
                       move "U?" to ws-unix-flavor
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23250-unix.
           add 1 to ws-page-cnt

           display "UNIX Skill                                          
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 " ws-unix-flavor "++++ I am the sysadmin. If you 
      -        "try and crack my machine don't be"
           end-display
           display "	      surprised if the municipal works department g
      -        "ets an 'accidental'"
           end-display
           display "          computer-generated order to start a new la
      -        "ndfill put on your front"
           end-display
           display "          lawn."
           end-display
           display " 2 " ws-unix-flavor "+++  I don't need to crack /etc
      -        "/passwd because I just modified su"
           end-display
           display "	      so that it doesn't prompt me.  The admin staf
      -        "f doesn't even know"
           end-display
           display "          I'm here. If you don't understand what I j
      -        "ust said, this category"
           end-display
           display "          does NOT apply to you!" end-display
           display " 3 " ws-unix-flavor "++   I've get the entire admin 
      -        "ticked off at me because I am always"
           end-display
           display "	      using all of the CPU time and trying to run p
      -        "rograms that I don't have"
           end-display
           display "          access to. I'm going to try cracking /etc/
      -        "passwd next week, just "
           end-display
           display "          don't tell anyone." end-display
           display " 4 " ws-unix-flavor "+    I not only have a unix acc
      -        "ount, but I slam VMS any chance I get."
           end-display
           display " 5 " ws-unix-flavor "     I have a unix account to d
      -        "o my stuff in."
           end-display
           display " 6 " ws-unix-flavor "-    I have a VMS account."
           end-display
           display " 7 " ws-unix-flavor "--   I've seen unix and didn't 
      -        "like it.  DEC rules!"
           end-display
           display " 8 " ws-unix-flavor "---  Unix geeks are actually ne
      -        "rds in disguise. "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your UNIX Skill code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "++++" to ws-unix-guru
                   else if ws-entry = 02 then
                       move "+++" to ws-unix-guru
                   else if ws-entry = 03 then
                       move "++" to ws-unix-guru
                   else if ws-entry = 04 then
                       move "+" to ws-unix-guru
                   else if ws-entry = 05 then
                       move " " to ws-unix-guru
                   else if ws-entry = 06 then
                       move "-" to ws-unix-guru
                   else if ws-entry = 07 then
                       move "--" to ws-unix-guru
                   else if ws-entry = 08 then
                       move "---" to ws-unix-guru
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23300-perl.
           add 1 to ws-page-cnt

           display "Perl                                                
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 P++++  I don't write Perl, I speak it. Perl has s
      -        "uperseded all"
           end-display
           display "          other programming languages. I firmly bel
      -        "ieve that all"
           end-display
           display "          programs can be reduced to a Perl one-line
      -        "r."
           end-display
           display " 2 P+++   Perl is a very powerful programming tool. 
      -        "Not only do I"
           end-display
           display "          no longer write shell scripts, I also no l
      -        "onger use awk or"
           end-display
           display "          sed. I use Perl for all programs of less t
      -        "han a thousand lines.  "
           end-display
           display " 3 P++    Perl is a powerful programming tool. I don
      -        "'t write shell"
           end-display
           display "          scripts anymore because I write them in Pe
      -        "rl."
           end-display
           display " 4 P+     I know of Perl. I like Perl. I just haven'
      -        "t learned much Perl,"
           end-display
           display "          but it is on my agenda. " end-display
           display " 5 P-     What's Perl got that awk and sed don't hav
      -        "e?  "
           end-display
           display " 6 P--    Perl users are sick, twisted programmers w
      -        "ho are just"
           end-display
           display "          showing off. " end-display
           display " 7 P---   Perl combines the power of sh, the clarity
      -        " of sed, and the"
           end-display
           display "          performance of awk with the simplicity of 
      -        "C. It should be banned."
           end-display
           display " 8 P?     What's Pearl?" end-display
           display " 9 !P     Our paranoid admin won't let us install pe
      -        "rl! Says it's"
           end-display
           display "          a 'hacking tool'."  end-display
           display " " end-display
           display "Enter your Perl code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "P++++" to ws-perl
                   else if ws-entry = 02 then
                       move "P+++" to ws-perl
                   else if ws-entry = 03 then
                       move "P++" to ws-perl
                   else if ws-entry = 04 then
                       move "P+" to ws-perl
                   else if ws-entry = 05 then
                       move "P-" to ws-perl
                   else if ws-entry = 06 then
                       move "P--" to ws-perl
                   else if ws-entry = 07 then
                       move "P---" to ws-perl
                   else if ws-entry = 08 then
                       move "P?" to ws-perl
                   else if ws-entry = 09 then
                       move "!P" to ws-perl
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23400-linux.
           add 1 to ws-page-cnt

           display "Linux                                               
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 L++++  I am Linus, hear me roar." end-display
           display " 2 L+++   I am a Linux wizard. I munch C code for br
      -        "eakfast and have enough "
           end-display
           display "          room left over for a kernel debugging. I h
      -        "ave so many patches "
           end-display
           display "          installed that I lost track about ten vers
      -        "ions ago. Linux"
           end-display
           display "          newbies consider me a net.god."
           end-display
           display " 3 L++    I use Linux almost exclusively on my syste
      -       "m. I monitor "
           end-display
           display "          comp.os.linux.* and even answer questions
      -        "some times. I've aliased "
           end-display
           display "          Linux FTP sites to make getting new softwa
      -        "re easier.  "
           end-display
           display " 4 L+     I've managed to get Linux installed and ev
      -        "en used it a few times. "
           end-display
           display "          It seems like it is just another OS."
           end-display
           display " 5 L      I know what Linux is, but that's about all
      -        "."
           end-display
           display " 6 L-     I have no desire to use Linux and frankly
      -       " don't give a rat's patootie "
           end-display
           display "          about it.  " end-display
           display " 7 L--    Unix sucks. Because Linux = Unix. Linux
      -        "sucks. I worship Bill Gates."
           end-display
           display " 8 L---   I am Bill Gates. " end-display
           display " 9 !L     I don't even know what Linux is!"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Linux code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "L++++" to ws-linux
                   else if ws-entry = 02 then
                       move "L+++" to ws-linux
                   else if ws-entry = 03 then
                       move "L++" to ws-linux
                   else if ws-entry = 04 then
                       move "L+" to ws-linux
                   else if ws-entry = 05 then
                       move "L" to ws-linux
                   else if ws-entry = 06 then
                       move "L-" to ws-linux
                   else if ws-entry = 07 then
                       move "L--" to ws-linux
                   else if ws-entry = 08 then
                       move "L---" to ws-linux
                   else if ws-entry = 09 then
                       move "!L" to ws-linux
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23500-386bsd.
           add 1 to ws-page-cnt

           display "386BSD                                              
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-displaY
           display "====================================================
      -"==========================="
           end-display
           display " 1 3+++   I am a 386BSD wizard. I munch C code for b
      -        "reakfast and have enough"
           end-display
           DISPLAY "          room left over for a kernel debugging. I h
      -        "ave so many patches"
           end-display
           display "          installed that I lost track about ten vers
      -        "ions ago. "
           end-display
           display "          386BSD newbies consider me a net.god."
           end-display
           display " 2 3++    I use 386BSD almost exclusively on my syst
      -       "em. I monitor "
           end-display
           display "          comp.os.386bsd.* and even answer questions
      -        "some times. I've aliased "
           end-display
           display "          386BSD FTP sites to make getting new softw
      -        "are easier.  "
           end-display
           display " 3 3+     I've managed to get 386BSD installed and e
      -        "ven used it a few times. "
           end-display
           display "          It seems like it is just another OS."
           end-display
           display " 4 3      I know what 386BSD is, but that's about al
      -        "l."
           end-display
           display " 5 3-     I have no desire to use 386BSD and frankly
      -       " don't give a rat's patootie "
           end-display
           display "          about it.  " end-display
           display " 6 3--    Unix sucks. Because 386BSD = Unix. 386BSD 
      -        "Sucks. I worship Bill Gates."
           end-display
           display " 7 3---   I am USL's lawyer. " end-display
           display " 8 !3     I don't even know what Linux is!"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your 386BSD code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "3+++" to ws-386bsd
                   else if ws-entry = 02 then
                       move "3++" to ws-386bsd
                   else if ws-entry = 03 then
                       move "3+" to ws-386bsd
                   else if ws-entry = 04 then
                       move "3" to ws-386bsd
                   else if ws-entry = 05 then
                       move "3-" to ws-386bsd
                   else if ws-entry = 06 then
                       move "3--" to ws-386bsd
                   else if ws-entry = 07 then
                       move "3---" to ws-386bsd
                   else if ws-entry = 08 then
                       move "!3" to ws-386bsd
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23600-news.
           add 1 to ws-page-cnt

           display "USENET                                              
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 N++++  I am Tim Pierce." end-display
           display " 2 N+++   I read so many news groups that the next b
      -        "atch of news "
           end-display
           display "          comes in before I finish reading the last 
      -        "batch, and I"
           end-display
           display "          have to read for about 2 hours straight be
      -        "fore I'm "
           end-display
           display "          caught up on the morning's news. Then ther
      -        "e's the afternoon...  "
           end-display
           display " 3 N++    I read all the news in a select handful of
      -        " groups. "
           end-display
           display " 4 N+     I read news recreationally when I have som
      -        "e time to kill."
           end-display
           display " 5 N      Usenet News? Sure, I read that once."
           end-display
           display " 6 N-     News is a waste of my time and I avoid it 
      -        "completely."
           end-display
           display " 7 N--    News sucks! 'Nuff said." end-display
           display " 8 N*     All I do is read news." end-display
           display " 9 !N     We don't have news." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Usenet code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-iF

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "N++++" to ws-news
                   else if ws-entry = 02 then
                       move "N+++" to ws-news
                   else if ws-entry = 03 then
                       move "N++" to ws-news
                   else if ws-entry = 04 then
                       move "N+" to ws-news
                   else if ws-entry = 05 then
                       move "N" to ws-news
                   else if ws-entry = 06 then
                       move "N-" to ws-news
                   else if ws-entry = 07 then
                       move "N--" to ws-news
                   else if ws-entry = 08 then
                       move "N*" to ws-news
                   else if ws-entry = 09 then
                       move "!N" to ws-news
                  end-if
              end-if
          end-if.

           perform 90000-clear-screen.

       23700-web.
           add 1 to ws-page-cnt

           display "World Wide Web                                      
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 W+++  I am a WebMaster. Don't even think about t
      -        "rying to view"
           end-display
           display "         my homepage without the latest version of N
      -        "etscape. When"
           end-display
           display "         I'm not on my normal net connection, I surf
      -        " the web using"
           end-display
           display "         my Newton and a cellular modem."
           end-display
           display " 2 W++   I have a homepage. I surf daily. My homepag
      -        "e is advertised"
           end-display
           display "         in my .signature."
           end-display
           display " 3 W+    I have the latest version of Netscape, and 
      -        "wander the web"
           end-display
           display "         only when there's something specific I'm lo
      -        "oking for."
           end-display
           display " 4 W     I have a browser and a connection. Occasio
      -        "nally I'll use them."
           end-display
           display " 5 W-    The web is really a pain. Life was so much
      -        " easier when"
           end-display
           display "         you could transfer information by simple 
      -        "ASCII. Now everyone"
           end-display
           display "         won't even consider your ideas unless you 
      -        "spiff them up"
           end-display
           display "         with bandwidth-consuming pictures and poin
      -        "tless information links. "
           end-display
           display " 6 W--   A pox on the Web! It wastes time and bandw
      -        "idth and just"
           end-display
           display "         gives the uneducated morons a reason to 
      -        "clutter the Internet."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your World Wide Web code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 06
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "W+++" to ws-web
                   else if ws-entry = 02 then
                       move "W++" to ws-web
                   else if ws-entry = 03 then
                       move "W+" to ws-web
                   else if ws-entry = 04 then
                       move "W" to ws-web
                   else if ws-entry = 05 then
                       move "W" to ws-web
                   else if ws-entry = 06 then
                       move "W--" to ws-web
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23800-emacs.
           ADD 1 TO WS-PAGE-CNT

           display "Emacs                                               
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 E+++   Emacs is my login shell!! M-x doctor is my
      -         " psychologist! I use"
           end-display
           display "          emacs to control my TV and toaster oven! A
      -        "ll you vi people"
           end-display
           display "          don't know what you're missing! I read alt
      -        ".relgion.emacs,"
           end-display
           display "          alt.sex.emacs, and comp.os.emacs."
           end-display
           display " 2 E++    I know and use elisp regularly!"
           end-display
           display " 3 E+     Emacs is great! I read my mail and news wi
      -        "th it!"
           end-display
           display " 4 E      Yeah, I know what emacs is, and use it as
      -        "my regular editor."
           end-display
           display " 5 E-     Emacs is too big and bloated for my tastes
      -        "."
           end-display
           display " 6 E--    Emacs is just a fancy word processor."
           end-display
           display " 7 E---   Emacs sucks! vi forever!!!"
           end-display
           display " 8 E----  Emacs sucks! pico forever!!!"
           end-display
           display " 9 !E     Emacs? What's that?" end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Emacs code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "E+++" to ws-emacs
                   else if ws-entry = 02 then
                       move "E++" to ws-emacs
                   else if ws-entry = 03 then
                       move "E+" to ws-emacs
                   else if ws-entry = 04 then
                       move "E" to ws-emacs
                   else if ws-entry = 05 then
                       move "E-" to ws-emacs
                   else if ws-entry = 06 then
                       move "E--" to ws-emacs
                   else if ws-entry = 07 then
                       move "E---" to ws-emacs
                   else if ws-entry = 08 then
                       move "E----" to ws-emacs
                   else if ws-entry = 09 then
                       move "!E" to ws-emacs
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       23900-kibo.
           add 1 to ws-page-cnt

           display "Kibo                                                
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 K++++++  I _am_ Kibo. " end-display
           display " 2 K+++++   I've had sex with Kibo." end-display
           display " 3 K++++    I've met Kibo." end-display
           display " 4 K+++     I've gotten mail from Kibo." end-display
           display " 5 K++      I've read Kibo." end-display
           display " 6 K+       I like Kibo." end-display
           display " 7 K        I know who Kibo is." end-display
           display " 8 K-       I don't know who Kibo is." end-display
           display " 9 K--      I dislike Kibo." end-display
           display "10 K---     I am Xibo." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Kibo code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "K++++++" to ws-kibo
                   else if ws-entry = 02 then
                       move "K+++++" to ws-kibo
                   else if ws-entry = 03 then
                       move "K++++" to ws-kibo
                   else if ws-entry = 04 then
                       move "K+++" to ws-kibo
                   else if ws-entry = 05 then
                       move "K++" to ws-kibo
                   else if ws-entry = 06 then
                       move "K+" to ws-kibo
                   else if ws-entry = 07 then
                       move "K" to ws-kibo
                   else if ws-entry = 08 then
                       move "K-" to ws-kibo
                   else if ws-entry = 09 then
                       move "K--" to ws-kibo
                   else if ws-entry = 10 then
                       move "K---" to ws-kibo
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24000-ms.
           add 1 to ws-page-cnt

           display "Microsoft Windows                                   
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 w++++    I have Windows, Windows NT, and Windows
      -        "NT Advanced Server all "
           end-display
           display "            running on my SMP RISC machine. I haven'
      -        "t seen daylight in six"
           end-display
           display "            months." end-display
           display " 2 w+++     I am a MS Windows programming god. I wro
      -        "te a VxD driver to allow "
           end-display
           display "            MS Windows and DOS to share the use of m
      -        "y waffle iron. "
           end-display
           display "            P.S. Unix sux. " end-display
           display " 3 W++      I write MS Windows programs in C and thi
      -        "nk about using C++ "
           end-display
           display "            someday. I've written at least one DLL."
           end-display
           display " 4 w+       I have installed my own custom sounds, w
      -        "allpaper, and screen "
           end-display
           display "            savers so my PC walks and talks like a f
      -        "un house.  Oh yeah, I have "
           end-display
           display "            a hundred TrueType(tm) fonts that I've i
      -        "nstalled but never used. "
           end-display
           display " 5 w        Ok, so I use MS Windows, I don't have to
      -        " like it. "
           end-display
           display " 6 w-       I'm still trying to install MS Windows a
      -        "nd have at least one "
           end-display
           display "            peripheral that never works right."
           end-display
           display " 7 w--      MS Windows is a joke operating system. H
      -        "ell, its not even an "
           end-display
           display "            operating system. NT is Not Tough enough
      -        " for me either. "
           end-display
           display " 8 w---     Windows has set back the computing indus
      -        "try by at least 10 "
           end-display
           display "            years. Bill Gates should be drawn, quart
      -       "ered, hung, shot, poisoned, "
           end-display
           display "            disemboweled, and then REALLY hurt."
           end-display
           display " 9 !w       I don't do Windows. Got a problem with t
      -        "hat? "
           end-display
           display " " end-display
           display "Enter your Microsoft Windows code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "w++++" to ws-ms
                   else if ws-entry = 02 then
                       move "w+++" to ws-ms
                   else if ws-entry = 03 then
                       move "w++" to ws-ms
                   else if ws-entry = 04 then
                       move "w+" to ws-ms
                   else if ws-entry = 05 then
                       move "w" to ws-ms
                   else if ws-entry = 06 then
                       move "w-" to ws-ms
                   else if ws-entry = 07 then
                       move "w--" to ws-ms
                   else if ws-entry = 08 then
                       move "w---" to ws-ms
                   else if ws-entry = 09 then
                       move "!w" to ws-ms
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24100-mac.
           add 1 to ws-page-cnt

           display "Macintosh                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 M++    I am a Mac guru. Anything those dos putzes
      -        " and unix nerds"
           end-display
           display "          can do, I can do better, and if not, I'll
      -        " write the damn"
           end-display
           display "          software to do it. " end-display
           display " 2 M+     A Mac has it's uses and I use it quite oft
      -        "en."
           end-display
           display " 3 M      I use a Mac, but I'm pretty indifferent ab
      -        "out it."
           end-display
           display " 4 M-     Macs suck. All real geeks have a character
      -        " prompt."
           end-display
           display " 5 M--    Macs do more than suck. They make a user 
      -        "stupid by allowing"
           end-display
           display "          them to use the system without knowing wha
      -        "t they are doing."
           end-display
           display "          Mac weenies have lower IQs than the fuzz i
      -        "n my navel."
           end-display
           display " 6 !M     What's a Macintosh?" end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Macintosh code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 06
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "M++" to ws-mac
                   else if ws-entry = 02 then
                       move "M+" to ws-mac
                   else if ws-entry = 03 then
                       move "M" to ws-mac
                   else if ws-entry = 04 then
                       move "M-" to ws-mac
                   else if ws-entry = 05 then
                       move "M--" to ws-mac
                   else if ws-entry = 06 then
                       move "!M" to ws-mac
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24200-vms.
           add 1 to ws-page-cnt

           display "VMS                                                 
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 V++    Unix is a passing fad compared to the real
      -        " power in the universe, "
           end-display
           display "          my VMS system.  " end-display
           display " 2 V+     I tend to like VMS better than Unix."
           end-display
           display " 3 V      I've used VMS." end-display
           display " 4 V-     Unix is much better than VMS for my comput
      -        "ing needs. "
           end-display
           display " 5 V--    I would rather smash my head repeatedly in
      -        "to a brick wall than "
           end-display
           display "          suffer the agony of working with VMS. It's
      -        "reminiscent of a dead "
           end-display
           display "          and decaying pile of moose droppings. Unix
      -        " rules the universe. "
           end-display
           display " 6 !V     I've not ever used VMS." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your VMS code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 06
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "V++" to ws-vms
                   else if ws-entry = 02 then
                       move "V+" to ws-vms
                   else if ws-entry = 03 then
                       move "V" to ws-vms
                   else if ws-entry = 04 then
                       move "V-" to ws-vms
                   else if ws-entry = 05 then
                       move "V--" to ws-vms
                   else if ws-entry = 06 then
                       move "!V" to ws-vms
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24400-pol.
           add 1 to ws-page-cnt

           display "Politics                                            
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 po+++  Fuckin' Minorities! Adolf Hitler is my her
      -        "o! And so is"
           end-display
           display "          Rush Limbaugh!"
           end-display
           display " 2 po++   All in favor of eliminating free speech, s
      -        "ay aye!"
           end-display
           display " 3 po+    Let's get the government off of big-busine
      -        "ss' back."
           end-display
           display " 4 po     Politics? I've heard of that somewhere but
      -        " in all honesty I "
           end-display
           display "          really don't give a shit. "
           end-display
           display " 5 po-    Bring back the 60's."
           end-display
           display " 6 po--   I'm still living in the 60's."
           end-display
           display " 7 po---  No taxes through no government."
           end-display
           display " 8 -po+   Don't label me you moron! Both sides are e
      -        "qually fucked up!"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Politics code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "po+++" to ws-pol
                   else if ws-entry = 02 then
                       move "po++" to ws-pol
                   else if ws-entry = 03 then
                       move "po+" to ws-pol
                   else if ws-entry = 04 then
                       move "po" to ws-pol
                   else if ws-entry = 05 then
                       move "po-" to ws-pol
                   else if ws-entry = 06 then
                       move "po--" to ws-pol
                   else if ws-entry = 07 then
                       move "po---" to ws-pol
                   else if ws-entry = 08 then
                       move "-po+" to ws-pol
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24500-cp.
           add 1 to ws-page-cnt

           display "Cypherpunk                                          
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 Y+++   I am T.C. May" end-display
           display " 2 Y++    I am on the cypherpunks mailing list and a
      -        "ctive around"
           end-display
           display "          Usenet. I never miss an opportunity to tal
      -        "k about the"
           end-display
           display "          evils of Clipper and the NSA. Orwells' 198
      -        "4 is more than"
           end-display
           display "          a story, it is a warning to ours' and futu
      -        "re generations."
           end-display
           display "          I'm a member of the EFF." end-display
           display " 3 Y+     I have an interest and concern in privacy 
      -        "issues, but in"
           end-display
           display "          reality I am not really all that active or
      -       " vocal."
           end-display
           display " 4 Y      I'm pretty indifferent on the whole issue"
           end-display
           display " 5 Y-     It seems to me that all of these concerns 
      -        "are a little "
           end-display
           display "          extreme. I mean, the government must be ab
      -        "le to protect"
           end-display
           display "          itself from criminals." end-display
           display " 6 Y--    Get a life. The only people that need this
      -        " kind of protection"
           end-display
           display "          are people with something to hide. I think
      -        " cypherpunks are "
           end-display
           display "          just a little paranoid."
           end-display
           display " 7 Y---   I am L. Dietweiller." end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Cypherpunk code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "Y+++" to ws-cp
                   else if ws-entry = 02 then
                       move "Y++" to ws-cp
                   else if ws-entry = 03 then
                       move "Y+" to ws-cp
                   else if ws-entry = 04 then
                       move "Y" to ws-cp
                   else if ws-entry = 05 then
                       move "Y-" to ws-cp
                   else if ws-entry = 06 then
                       move "Y--" to ws-cp
                   else if ws-entry = 07 then
                       move "Y---" to ws-cp
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24700-trek.
           add 1 to ws-page-cnt

           display "Star Trek                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 t+++ It's not just a TV show, its a religion. I k
      -        "now all about"
           end-display
           display "        warp field dynamics and the principles behin
      -        "d the transporter."
           end-display
           display "        I have memorized the TECH manual. I speak Kl
      -        "ingon. I go to"
           end-display
           display "        cons with Vulcan ears on. I have no life. It
      -        "'s not just a "
           end-display
           display "        TV show, its a religion. " end-display
           display " 2 t++  It's the best show around. I have all the ep
      -        "isodes and the"
           end-display
           display "        movies on tape and can quote entire scenes v
      -        "erbatim.  I've "
           end-display
           display "        built a few of the model kits too. But you l
      -        "l never catch"
           end-display
           display "        me at one of those conventions. Those people
      -        " are kooks."
           end-display
           display " 3 t+   It's a damn fine TV show and is one of the o
      -        "nly things "
           end-display
           display "        good on television any more. " end-display
           display " 4 t    It's just another TV show." end-display
           display " 5 t-   Maybe it is just me, but I have no idea what
      -        " the big deal"
           end-display
           display "        with Star Trek is. Perhaps I'm missing somet
      -        "hing but I "
           end-display
           display "        just think it is bad drama. " end-display
           DISPLAY " 6 t--  Star Trek is just another Space Opera."
           end-display
           display " 7 t--- Star Trek SUCKS! It is the worst crap I ha
      -        "ve ever seen!"
           end-display
           display "        Hey, all you trekkies out there, GET A LIFE
      -        "!!!"
           end-display
           display " " end-display
           display "Enter your Star Trek code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "t+++" to ws-trek
                   else if ws-entry = 02 then
                       move "t++" to ws-trek
                   else if ws-entry = 03 then
                       move "t+" to ws-trek
                   else if ws-entry = 04 then
                       move "t" to ws-trek
                   else if ws-entry = 05 then
                       move "t-" to ws-trek
                   else if ws-entry = 06 then
                       move "t--" to ws-trek
                   else if ws-entry = 07 then
                       move "t---" to ws-trek
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       24800-bab.
           add 1 to ws-page-cnt

           display "Babylon 5                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           END-DISPLAY
           display "====================================================
      -"==========================="
           end-display
           display " 1 5+++   I am a True Worshipper of the Church of Jo
      -        "e who lives"
           end-display
           display "          eats breathes and thinks Babylon 5, and ha
      -        "s Evil thoughts"
           end-display
           display "          about stealing Joe's videotape archives ju
      -        "st to see "
           end-display
           display "          episodes earlier." end-display
           display " 2 5++    Finally a show that shows what a real futu
      -        "re would look"
           end-display
           display "          like. None of this Picardian 'Let's talk a
      -        "bout it and"
           end-display
           display "          be friends' crap. And what's this? We fina
      -        "lly get to "
           end-display
           display "          see a bathroom!  Over on that Enterprise, 
      -        "they've been "
           end-display
           display "          holding it for over seven years.  "
           end-display
           display " 3 5+     Babylon 5 certainly presents a fresh persp
      -        "ective in the"
           end-display
           display "          Sci-Fi universe. I watch it weekly."
           end-display
           display " 4 5      I've seen it, I am pretty indifferent to i
      -        "t."
           end-display
           display " 5 5-     This show is sub-par. The acting is wooden
      -        ", the special "
           end-display
           display "          effects are obviously poor quality. In gen
      -        "eral, it"
           end-display
           display "          seems like a very cheap Star Trek ripoff."
           end-display
           display " 6 5--    You call this Sci-Fi? That is such a load 
      -        "of crap! This"
           end-display
           display "          show is just a soap with bad actors, piss-
      -        "poor effects,"
           end-display
           display "          and lame storylines. Puh-leese."
           end-display
           display " 7 !5     I've never seen Babylon 5."
           end-display
           display " " end-display
           display "Enter your Babylon 5 code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "5+++" to ws-bab
                   else if ws-entry = 02 then
                       move "5++" to ws-bab
                   else if ws-entry = 03 then
                       move "5+" to ws-bab
                   else if ws-entry = 04 then
                       move "5" to ws-bab
                   else if ws-entry = 05 then
                       move "5-" to ws-bab
                   else if ws-entry = 06 then
                       move "5--" to ws-bab
                   else if ws-entry = 07 then
                       move "!5" to ws-bab
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25000-jeop.
           add 1 to ws-page-cnt

           display "Jeopardy                                            
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 j+++   I dress like Art Fleming, practice Alex Tr
      -        "ebek's vocal"
           end-display
           display "          nuances, and make a pilgrimage to the Jeop
      -        "ardy studio"
           end-display
           display "          every six months to either take the contes
      -        "tant test or"
           end-display
           display "          to cheer from the audience. "
           end-display
           display " 2 j++    I watch Jeopardy regularly, and annoy othe
      -        "rs in the college"
           end-display
           display "          rec center by shouting out the answers."
           end-display
           display " 3 j+     I watch Jeopardy regularly."
           end-display
           display " 4 j      Sure I watch it, but, hey, it's only a sho
      -        "w."
           end-display
           display " 5 j-     Jeopardy? That's show's for a bunch of no-
      -        "life eggheads. "
           end-display
           display " 6 j--    I annoy others in the college rec center b
      -        "y shouting out"
           end-display
           display "          the *wrong* answers." end-display
           display " 7 !j     I've never seen Jeopardy or don't watch it
      -        "."
           end-display
           display " 8 j$     I've won money on the show." end-display
           display " 9 jP     I've gotten the d*mn Lee Press-On Nails on
      -        " the show (or"
           end-display
           display "          some other lame-o consolation prize). "
           end-display
           display "10 jx     I don't watch Jeopardy because it's too ea
      -        "sy."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Jeopardy code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "j+++" to ws-jeop
                   else if ws-entry = 02 then
                       move "j++" to ws-jeop
                   else if ws-entry = 03 then
                       move "j+" to ws-jeop
                   else if ws-entry = 04 then
                       move "j" to ws-jeop
                   else if ws-entry = 05 then
                       move "j-" to ws-jeop
                   else if ws-entry = 06 then
                       move "j--" to ws-jeop
                   else if ws-entry = 07 then
                       move "!j" to ws-jeop
                   else if ws-entry = 08 then
                       move "j$" to ws-jeop
                   else if ws-entry = 09 then
                       move "jP" to ws-jeop
                   else if ws-entry = 10 then
                       move "jx" to ws-jeop
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25100-role.
           add 1 to ws-page-cnt

           display "Role Playing                                        
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 R+++   I've written and publish my own gaming mat
      -        "erials. "
           end-display
           display " 2 R++    There is no life outside the role of the d
      -        "ie. I know all"
           end-display
           display "          of piddly rules of (chosen game). _MY_ own
      -        " warped rules"
           end-display
           display "          scare the rest of the players."
           end-display
           display " 3 R+     I've got my weekly sessions set up and a c
      -        "haracter that"
           end-display
           display "          I know better than I know myself. "
           end-display
           display " 4 R      Role-Playing? That's just something to do
      -        " to kill a"
           end-display
           display "          Saturday afternoon." end-display
           display " 5 R-     Gosh, what an utter waste of time!"
           end-display
           display " 6 R--    Role-Players are instruments of pure evil"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Role Playing code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry < 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "R+++" to ws-role
                   else if ws-entry = 02 then
                       move "R++" to ws-role
                   else if ws-entry = 03 then
                       move "R+" to ws-role
                   else if ws-entry = 04 then
                       move "R" to ws-role
                   else if ws-entry = 05 then
                       move "R-" to ws-role
                   else if ws-entry = 06 then
                       move "R--" to ws-role
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25200-magic.
           add 1 to ws-page-cnt

           display "MAGIC: The Gathering                                
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 G++++  I am considered a Magic(tm) god. I have
      -        "nicknames for"
           end-display
           display "          every card and know just about every strat
      -        "egy there is."
           end-display
           display " 2 G+++   I have a Lord of the Pit, a Black Lotus an
      -        "d a Reverse"
           end-display
           display "          Damage. I play for hours every night."
           end-display
           display " 3 G++    I've spent almost $100 on cards. A good ch
      -        "unk of my"
           end-display
           display "          spare time goes into playing or constructi
      -        "ng decks and"
           end-display
           display "          keeping up my checklist." end-display
           display " 4 G+     Ok, ok, so I bought a few packs of cards.
      -        " Big deal."
           end-display
           display " 5 G      I play Magic, if I can borrow a deck. It's
      -        " an ok game."
           end-display
           display " 6 G-     I don't even play anymore. I just collect.
      -       " My cards fill "
           end-display
           display "          three shoeboxes." end-display
           display " 7 G--    I don't go to class/work anymore.  Sometim
      -        "es I don't sleep."
           end-display
           display " 8 G---   I have 3 Lords of the Pit, Armageddon, Wra
      -        "th of God,"
           end-display
           display "          and two Reverse Damages. I also have all f
      -        "ive of the"
           end-display
           display "          Greater Legends Dragons. I can quote the e
      -        "xact wording"
           end-display
           display "          and, in some cases, casting cost, of any c
      -        "ard on demand."
           end-display
           display "          I've memorized the PPG. I am a Magic munch
      -        "kin. "
           end-display
           display " 9 G----  Some friends and I are trying to get boxes
      -        " of booster"
           end-display
           display "          packs at cost so we can sell them at a pro
      -        "fit and buy more"
           end-display
           display "          cards at cost that we can sell for profit 
      -        "and buy more"
           end-display
           display "          cards at.... " end-display
           display "10 G?     What the hell _IS_ Magic?"
           end-display
           display " " end-display
           display "Enter your MAGIC code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "G++++" to ws-magic
                   else if ws-entry = 02 then
                       move "G+++" to ws-magic
                   else if ws-entry = 03 then
                       move "G++" to ws-magic
                   else if ws-entry = 04 then
                       move "G+" to ws-magic
                   else if ws-entry = 05 then
                       move "G" to ws-magic
                   else if ws-entry = 06 then
                       move "G-" to ws-magic
                   else if ws-entry = 07 then
                       move "G--" to ws-magic
                   else if ws-entry = 08 then
                       move "G---" to ws-magic
                   else if ws-entry = 09 then
                       move "G----" to ws-magic
                   else if ws-entry = 10 then
                       move "G?" to ws-magic
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25300-tv.
           add 1 to ws-page-cnt

           display "Television                                          
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 tv+++  There's nothing I can experience 'out ther
      -        "e' that I can't"
           end-display
           display "          see coming over my satellite dish. I wish 
      -        "there were"
           end-display
           display "          MORE channels. " end-display
           display " 2 tv++   I just leave the tv on, to make sure I don
      -        "'t miss anything."
           end-display
           display " 3 tv+    I watch some tv every day. "
           end-display
           display " 4 tv     I watch only the shows that are actually w
      -        "orth while."
           end-display
           display " 5 tv-    I watch tv for the news and 'special progr
      -        "amming.' "
           end-display
           display " 6 tv--   I turn my tv on during natural disasters."
           end-display
           display " 7 !tv    I do not own a television. " end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Television code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 07
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "tv+++" to ws-tv
                   else if ws-entry = 02 then
                       move "tv++" to ws-tv
                   else if ws-entry = 03 then
                       move "tv+" to ws-tv
                   else if ws-entry = 04 then
                       move "tv" to ws-tv
                   else if ws-entry = 05 then
                       move "tv-" to ws-tv
                   else if ws-entry = 06 then
                       move "tv--" to ws-tv
                   else if ws-entry = 07 then
                       move "!tv" to ws-tv
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25400-books.
           add 1 to ws-page-cnt

           display "Books                                               
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 b+++   I consume a few books a week as part of a 
      -        "staple diet."
           end-display
           display " 2 b++    I find the time to get through at least on
      -        "e new book a month. "
           end-display
           display " 3 b+     I enjoy reading, but don't get the time ve
      -        "ry often.  "
           end-display
           display " 4 b      I read the newspaper and the occasional bo
      -        "ok. "
           end-display
           display " 5 b-     I read when there is no other way to get t
      -        "he information. "
           end-display
           display " 6 b--    I did not actually READ the geek code, "
           end-display
           display "          I just had someone tell me. "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Books code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 06
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "b+++" to ws-books
                   else if ws-entry = 02 then
                       move "b++" to ws-books
                   else if ws-entry = 03 then
                       move "b+" to ws-books
                   else if ws-entry = 04 then
                       move "b" to ws-books
                   else if ws-entry = 05 then
                       move "b-" to ws-books
                   else if ws-entry = 06 then
                       move "b--" to ws-books
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25500-doom.
           add 1 to ws-page-cnt

           display "DOOM!                                               
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 D+++   I crank out PWAD files daily, complete wit
      -        "h new monsters,"
           end-display
           display "          weaponry, sounds and maps. I'm a DOOM God.
      -        " I can solve the"
           end-display
           display "          original maps in nightmare mode with my ey
      -        "es closed. "
           end-display
           display " 2 D++    I've played the shareware version and boug
      -        "ht the real one"
           end-display
           display "          and I'm actually pretty good at the game. 
      -        "I occasionally "
           end-display
           display "          download PWAD files and play them too. "
           end-display
           display " 3 D+     It's a fun, action game that is a nice div
      -        "ersion on a "
           end-display
           display "          lazy afternoon." end-display
           display " 4 D      I've played the game and I'm pretty indiff
      -        "erent."
           end-display
           display " 5 D-     I've played the game and really didn't thi
      -        "nk it was"
           end-display
           display "          all that impressive." end-display
           display " 6 D--    It's an overly-violent game and pure crap"
           end-display
           display " 7 D---   I've seen better on my Atari 2600."
           end-display
           display " 8 !D     I've never played Doom!" end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your DOOM code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "D+++" to ws-doom
                   else if ws-entry = 02 then
                       move "D++" to ws-doom
                   else if ws-entry = 03 then
                       move "D+" to ws-doom
                   else if ws-entry = 04 then
                       move "D" to ws-doom
                   else if ws-entry = 05 then
                       move "D-" to ws-doom
                   else if ws-entry = 06 then
                       move "D--" to ws-doom
                   else if ws-entry = 07 then
                       move "D---" to ws-doom
                   else if ws-entry = 08 then
                       move "!D" to ws-doom
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25600-barney.
           add 1 to ws-page-cnt

           display "Barney                                              
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 B+++   I worship the ground He walks on. I wish t
      -        "o erect a"
           end-display
           display "          shrine for Him in my front yard. I feel a
      -        " need to sell"
           end-display
           display "          all my worldly belongings, shave my head,
      -        "and go to "
           end-display
           display "          airports where I will hand out Barney doll
      -        "s and spread"
           end-display
           display "          His message of universal love for everyone
      -        " regardless of"
           end-display
           display "          race, creed, color, sexual preference, or 
      -        "species. "
           end-display
           display " 2 B++    I don't miss an episode, except when I hav
      -        "e to work"
           end-display
           display "          or go in for a root canal. Barney loves me
      -        ". "
           end-display
           display " 3 B+     I like him. He has a nice, wholesome messa
      -        "ge. He's"
           end-display
           display "          good for the country. " end-display
           display " 4 B      Hey, the little tykes love him, they don't
      -       " go around "
           end-display
           display "          karate-chopping each other any more; what'
      -        "s the big deal?"
           end-display
           display " 5 B-     Barney is annoying." end-display
           display " 6 B--    Don't talk to me about him. I'm getting si
      -        "ck of his "
           end-display
           display "          smarmy message. He makes me ill."
           end-display
           display " 7 B---   He's sick. He's polluting our children's m
      -       "inds with this"
           end-display
           display "          love and tolerance crap."
           end-display
           display " 8 !B     Who's Barney?" end-display
           display " " end-display
           display "Enter your Barney code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "B+++" to ws-barney
                   else if ws-entry = 02 then
                       move "B++" to ws-barney
                   else if ws-entry = 03 then
                       move "B+" to ws-barney
                   else if ws-entry = 04 then
                       move "B" to ws-barney
                   else if ws-entry = 05 then
                       move "B-" to ws-barney
                   else if ws-entry = 06 then
                       move "B--" to ws-barney
                   else if ws-entry = 07 then
                       move "B---" to ws-barney
                   else if ws-entry = 08 then
                       move "!B" to ws-barney
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25700-educ.
           add 1 to ws-page-cnt

           display "Education                                           
      -       "              age: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 e++++  Still pretty stupid, over qualified to wor
      -        "k any"
           end-display
           display "          job, went and got my Ph.D. "
           end-display
           display " 2 e+++   Had not learned enough to know better not 
      -        "to go back"
           end-display
           display "          and try for a master's degree."
           end-display
           display " 3 e++    Managed to finish my bachelors. " 
           end-display
           display " 4 e+     Started a degree, plan to finish it some d
      -        "ay.  "
           end-display
           display " 5 e      K-12, been on a college campus."
           end-display
           display " 6 e-     Got my bachelors, escaped alive, and am ma
      -        "king hoards"
           end-display
           display "          of money writing unmaintainable (except by
      -        " me) software. "
           end-display
           display " 7 e--    The company I work for was dumb enough to 
      -        "fund my way"
           end-display
           display "          through a masters degree, then started pay
      -        "ing me even more money."
           end-display
           display " 8 e---   Achieved a Ph.D, have devoted my life to i
      -        "nsignificant"
           end-display
           display "          research, which my employer pays dearly fo
      -        "r.  "
           end-display
           display " 9 !e     Flunked high school, learned life the hard
      -        " way."
           end-display
           display "10 e*     I learned everything there is to know abou
      -        "t life from"
           end-display
           display "          the 'Hitchhiker's Trilogy'. "
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Education code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "e++++" to ws-educ
                   else if ws-entry = 02 then
                       move "e+++" to ws-educ
                   else if ws-entry = 03 then
                       move "e++" to ws-educ
                   else if ws-entry = 04 then
                       move "e+" to ws-educ
                   else if ws-entry = 05 then
                       move "e" to ws-educ
                   else if ws-entry = 06 then
                       move "e-" to ws-educ
                   else if ws-entry = 07 then
                       move "e--" to ws-educ
                   else if ws-entry = 08 then
                       move "e---" to ws-educ
                   else if ws-entry = 09 then
                       move "!e" to ws-educ
                   else if ws-entry = 10 then
                       move "e*" to ws-educ
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25800-music.
           add 1 to ws-page-cnt

           display "Music                                               
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 u+++   I consider myself over-refined and grok th
      -        "at heavy-duty"
           end-display
           display "          elevator music." end-display
           display " 2 u++    I consider myself refined and enjoy classi
      -        "cal and"
           end-display
           display "          new-age selections." end-display
           display " 3 u+     I own a tape or CD collection (records als
      -        "o count,"
           end-display
           display "          but you would be admitting how old you rea
      -        "lly are)."
           end-display
           display " 4 u      I occasionally listen to the radio."
           end-display
           display " 5 u-     Just play it loud." end-display
           display " 6 u--    I play air-guitar better than anyone else.
      -    " "
           end-display
           display " 7 u---   LISTEN! I SAID TO PLAY IT LOUD!" 
           end-display
           display " 8 u*     I listen to music that no one else has eve
      -        "r heard of." 
           end-display
           display " 9 u**    I listen to so many types of music that I 
      -        "can't even"
           end-display
           display "          keep them straight." end-display
           display "10 -u     I like _both_ kinds of music: Country AND 
      -        "Western."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Music code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 10
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "u+++" to ws-music
                   else if ws-entry = 02 then
                       move "u++" to ws-music
                   else if ws-entry = 03 then
                       move "u+" to ws-music
                   else if ws-entry = 04 then
                       move "u" to ws-music
                   else if ws-entry = 05 then
                       move "u-" to ws-music
                   else if ws-entry = 06 then
                       move "u--" to ws-music
                   else if ws-entry = 07 then
                       move "u---" to ws-music
                   else if ws-entry = 08 then
                       move "u*" to ws-music
                   else if ws-entry = 09 then
                       move "e**" to ws-music
                   else if ws-entry = 10 then
                       move "-u" to ws-music
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       25900-house.
           add 1 to ws-page-cnt

           display "Housing                                             
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 h++    Living in a cave with 47 computers and an 
      -        "Internet feed,"
           end-display
           display "          located near a Dominoes pizza. See !d."
           end-display
           display " 2 h+     Living alone, get out once a week to buy f
      -        "ood, no more"
           end-display
           display "          than once a month to do laundry. All surfa
      -        "ces covered."
           end-display
           display " 3 h      Friends come over to visit every once in a
      -        " while to talk"
           end-display
           display "          about Geek things. There is a place for th
      -        "em to sit."
           end-display
           display " 4 h-     Living with one or more registered Geeks."
           end-display
           display " 5 h--    Living with one or more people who know no
      -        "thing about"
           end-display
           display "          being a Geek and refuse to watch 'Star Tre
      -        "k'. "
           end-display
           display " 6 h---   Married, with the potential for children. 
      -        " (persons living"
           end-display
           display "          with a fiance might as well label themselv
      -        "es h---,"
           end-display
           display "          you're as good as there already.)"
           end-display
           display " 7 h----  Married with children - Al Bundy can sympa
      -        "thize ."
           end-display
           display " 8 !h     I am stuck living with my parents!"
           end-display
           display " 9 h*     I'm not sure where I live anymore. This la
      -        "b/workplace"
           end-display
           display "          seems like home to me. " end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Housing code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "h++" to ws-house
                   else if ws-entry = 02 then
                       move "h+" to ws-house
                   else if ws-entry = 03 then
                       move "h" to ws-house
                   else if ws-entry = 04 then
                       move "h-" to ws-house
                   else if ws-entry = 05 then
                       move "h--" to ws-house
                   else if ws-entry = 06 then
                       move "h---" to ws-house
                   else if ws-entry = 07 then
                       move "h----" to ws-house
                   else if ws-entry = 08 then
                       move "!h" to ws-house
                   else if ws-entry = 09 then
                       move "h*" to ws-house
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26000-friends.
           add 1 to ws-page-cnt

           display "Friends                                             
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 f++    I have so many friends, I make other peopl
      -       "e jealous."
           end-display
           display " 2 f+     I have quite a few really close friends. W
      -        "e get along great. They "
           end-display
           display "          are all other geeks, though."
           end-display
           display " 3 f      Yeah, I have friends. Who told you?"
           end-display
           display " 4 f-     I have a few friends. They barely seem to 
      -        "speak to me anymore."
           end-display
           display " 5 f--    I've got about one friend left in the worl
      -        "d, who probably wants to "
           end-display
           display "          shoot me. " end-display
           display " 6 f---   I used to have friends, but I didn't like 
      -        "it ."
           end-display
           display " 7 f?     I *think* I have friends." end-display
           display " 8 f*     Everyone is my friend." end-display
           display " 9 !f     I have no friends. Get lost." 
           end-display
           display " " end-display
           display " " end-display
           display " " END-DISPLAY
           display "Enter your Friends code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if wS-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if wS-entry = 01 then
                       move "f++" to ws-friends
                   else if ws-entry = 02 then
                       move "f+" to ws-friends
                   else if ws-entry = 03 then
                       move "f" to ws-friends
                   else if ws-entry = 04 then
                       move "f-" to ws-friends
                   else if ws-entry = 05 then
                       move "f--" to ws-friends
                   else if ws-entry = 06 then
                       move "f---" to ws-friends
                   else if ws-entry = 07 then
                       move "f?" to ws-friends
                   else if ws-entry = 08 then
                       move "f*" to ws-friends
                   else if ws-entry = 09 then
                       move "!f" to ws-friends
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26100-rel.
           add 1 to ws-page-cnt

           display "Relationships                                       
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 r+++   Found someone, dated, and am now married."
           end-display
           display " 2 r++    I've dated my current SO for a long time."
           end-display
           display " 3 r+     I bounce from one relationship to another,
      -        " but I have quite a few. "
           end-display
           display " 4 r      I date periodically." end-display
           display " 5 r-     I have difficulty maintaining a relationsh
      -        "ip."
           end-display
           display " 6 r--    Most people aren't interested in dating me
      -        "." 
           end-display
           display " 7 r---   I'm beginning to think I'm a leper or some
      -        "thing, the way"
           end-display
           display "          people avoid me like the plague."
           end-display
           display " 8 !r     I've never had a relationship."
           end-display
           display " 9 r*     signifying membership in the SBCA (Sour Ba
      -        "chelor(ette)'s"
           end-display
           display "          Club of America). The motto is 'Bitter, bu
      -        "t not Desperate'."
           end-display
           display "          First founded at Caltech. " end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Relationships code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 09
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "r+++" to ws-rel
                   else if ws-entry = 02 then
                       move "r++" to ws-rel
                   else if ws-entry = 03 then
                       move "r+" to ws-rel
                   else if ws-entry = 04 then
                       move "r" to ws-rel
                   else if ws-entry = 05 then
                       move "r-" to ws-rel
                   else if ws-entry = 06 then
                       move "r--" to ws-rel
                   else if ws-entry = 07 then
                       move "r---" to ws-rel
                   else if ws-entry = 08 then
                       move "!r" to ws-rel
                   else if ws-entry = 09 then
                       move "r*" to ws-rel
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26200-nut.
           add 1 to ws-page-cnt

           display "Nutrition                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 n+++   I graze like a bunny - pass me a carrot!"
           end-display
           display " 2 n++    I like the fibers in food."
           end-display
           display " 3 n+     I like food - especially when it is health
      -        "y. "
           end-display
           display " 4 n-     Food? I just grab something from the shelv
      -        "es with meat in it. "
           end-display
           display " 5 n--    I eat only the cheap things - even with ar
      -        "tificial meat and"
           end-display
           display "          vegetables." end-display
           display " 6 n---   I eat meat - seen Jurassic Park?"
           end-display
           display " 7 n----  I _live_ on snacks and coke."
           end-display
           display " 8 !n     Eh what? never mind the menu, give me some
      -        "thing to eat!"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Nutrition code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 08
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "n+++" to ws-nut
                   else if ws-entry = 02 then
                       move "n++" to ws-nut
                   else if ws-entry = 03 then
                       move "n+" to ws-nut
                   else if ws-entry = 04 then
                       move "n-" to ws-nut
                   else if ws-entry = 05 then
                       move "n--" to ws-nut
                   else if ws-entry = 06 then
                       move "n---" to ws-nut
                   else if ws-entry = 07 then
                       move "n----" to ws-nut
                   else if ws-entry = 08 then
                       move "!n" to ws-nut
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26300-gender.
           add 1 to ws-page-cnt

           display "Gender                                              
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 x   I am female." end-display
           display " 2 y   I am male." end-display
           display " 3 z   Its none of your business what sex I am."
           end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display "Enter your Gender code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 03
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "x" to ws-sex-gender
                   else if ws-entry = 02 then
                       move "y" to ws-sex-gender
                   else if ws-entry = 03 then
                       move "z" to ws-sex-gender
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26350-sex.
           add 1 to ws-page-cnt

           display "Sexuality                                           
      -       "             Page: " ws-page-cnt " of " ws-tot-page
           end-display
           display "====================================================
      -"==========================="
           end-display
           display " 1 " ws-sex-gender "++++  I have a few little rug ra
      -        "ts to prove I've"
           end-display
           display "          been there. Besides, with kids around, who
      -        " has time for sex? "
           end-display
           display " 2 " ws-sex-gender "+++   I'm married, so I can get 
      -        "it"
           end-display
           display "          (theoretically) whenever I want."
           end-display
           display " 3 " ws-sex-gender "++    I was once referred to as 
      -        "'easy'."
           end-display
           display "          I have no idea where that might have come
      -        " from though."
           end-display
           display " 4 " ws-sex-gender "+     I've had real, live sex."
           display " 5 " ws-sex-gender "-     I prefer computer sex to r
      -        "eal sex."
           end-display
           display " 6 " ws-sex-gender "--    I was once referred to a
      -        "s a 'cyberslut',"
           end-display
           display "          but I have no idea where that might have c
      -        "ome from."
           end-display
           display " 7 " ws-sex-gender "*     I'm a pervert."
           end-display
           display " 8 " ws-sex-gender "**    I've been known to make pe
      -        "rverts look like angels."
           end-display
           display " 9 !" ws-sex-gender "     Sex? What's that? I've had
      -        " no sexual experiences."
           end-display
           display "10 " ws-sex-gender "?     It's none of your business
      -        " what my sex life is like."
           end-display
           display "11 +" ws-sex-gender "?    Sex? What's that? No exper
      -        "ience, willing to learn! "
           end-display
           display " " end-display
           display "Enter your Sexuality code "
               "number here [0 to quit]: " with no advancing
           end-display

           accept ws-entry end-accept

           if ws-entry (2:1) = space
               move ws-entry (1:1) to ws-entry (2:1)
               move 0              to ws-entry (1:1)   
           end-if

           if ws-entry not numeric
               continue 
           else
               if ws-entry <= 11
                   set ws-valid-flag to "y"
                   if ws-entry = 00 then
                       perform 92000-abend
                   else if ws-entry = 01 then
                       move "++++" to ws-sex
                   else if ws-entry = 02 then
                       move "+++" to ws-sex
                   else if ws-entry = 03 then
                       move "++" to ws-sex
                   else if ws-entry = 04 then
                       move "+" to ws-sex
                   else if ws-entry = 05 then
                       move "-" to ws-sex
                   else if ws-entry = 06 then
                       move "--" to ws-sex
                   else if ws-entry = 07 then
                       move "*" to ws-sex
                   else if ws-entry = 08 then
                       move "**" to ws-sex
                   else if ws-entry = 09 then
                       move "!" to ws-sex-pre
                       move " " to ws-sex
                   else if ws-entry = 10 then
                       move "?" to ws-sex
                   else if ws-entry = 11 then
                       move "+" to ws-sex-pre
                       move "?" to ws-sex
                   end-if
               end-if
           end-if.

           perform 90000-clear-screen.

       26500-print.
           move  "-----BEGIN GEEK CODE BLOCK-----" to   ws-head
           write geek-output-rec                   from ws-head 
           end-write
           add 1 to ws-rec-cnt
           write geek-output-rec                   from ws-ver
           end-write
           add 1 to ws-rec-cnt

           string ws-type           delimited by space  
                  space             delimited by size
                  ws-dress          delimited by space
                  space             delimited by size
                  ws-hair           delimited by space
                  space             delimited by size
                  ws-shape          delimited by space
                  space             delimited by size
                  ws-glasses        delimited by space
                  space             delimited by size
                  ws-pens           delimited by space
                  space             delimited by size
                  ws-auto           delimited by space
                  space             delimited by size
                  ws-age            delimited by space
                  space             delimited by size
                  ws-weird          delimited by space
                  space             delimited by size
                  ws-verbage        delimited by space
                  space             delimited by size
                  ws-comp           delimited by space
           into ws-print-line1
           end-string 

           write geek-output-rec from ws-print-line1
           end-write
           add 1 to ws-rec-cnt

           string ws-unix           delimited by space  
                  space             delimited by size
                  ws-perl           delimited by space  
                  space             delimited by size
                  ws-linux          delimited by space  
                  space             delimited by size
                  ws-386bsd         delimited by space  
                  space             delimited by size
                  ws-news           delimited by space  
                  space             delimited by size
                  ws-web            delimited by space  
                  space             delimited by size
                  ws-mac            delimited by space  
                  space             delimited by size
                  ws-vms            delimited by space  
                  space             delimited by size
                  ws-pol            delimited by space  
                  space             delimited by size
                  ws-cp             delimited by space  
           into ws-print-line2
           end-string 

           write geek-output-rec from ws-print-line2
           end-write
           add 1 to ws-rec-cnt

           string ws-trek           delimited by space
                  space             delimited by size
                  ws-bab            delimited by space
                  space             delimited by size
                  ws-jeop           delimited by space
                  space             delimited by size
                  ws-role           delimited by space
                  space             delimited by size
                  ws-magic          delimited by space
                  space             delimited by size
                  ws-emacs          delimited by space
                  space             delimited by size
                  ws-kibo           delimited by space
                  space             delimited by size
                  ws-ms             delimited by space
                  space             delimited by size
                  ws-tv             delimited by space
           into ws-print-line3
           end-string 

           write geek-output-rec from ws-print-line3
           end-write
           add 1 to ws-rec-cnt

           string ws-books          delimited by space
                  space             delimited by size
                  ws-doom           delimited by space
                  space             delimited by size
                  ws-barney         delimited by space
                  space             delimited by size
                  ws-educ           delimited by space
                  space             delimited by size
                  ws-music          delimited by space
                  space             delimited by size
                  ws-house          delimited by space
                  space             delimited by size
                  ws-rel            delimited by space
                  space             delimited by size
                  ws-friends        delimited by space
                  space             delimited by size
                  ws-nut            delimited by space  
                  space             delimited by size
                  ws-code-sex       delimited by size  
           into ws-print-line4
           end-string 

           write geek-output-rec from ws-print-line4
           end-write
           add 1 to ws-rec-cnt

           move  "-----END GEEK CODE BLOCK-----" to   ws-end
           write geek-output-rec                 from ws-end
           end-write
           add 1 to ws-rec-cnt

           display "-----BEGIN GEEK CODE BLOCK-----" end-display
           display "Version: 2.1" end-display
           display ws-print-line1 end-display
           display ws-print-line2 end-display
           display ws-print-line3 end-display
           display ws-print-line4 end-display
           display "------END GEEK CODE BLOCK------" end-display
           display " " end-display
           display " " end-display
           display " " end-display
           display ws-rec-cnt " records written to 'geekcode.sig'"
           end-display
           display " " end-display
           display " " end-display
           display " " end-display.

       30000-cleanup.
           close geek-sig
           perform 94000-terminate.

       90000-clear-screen.
           move "clear" to ws-command
           call "system" using ws-command giving ws-return-sys-code
               end-call
           display " " end-display
           display " " end-display.

       91000-print-heading.
           display "Geek Code Generator v0.2 - Generates your geek code"
           end-display
           display "Copyright (C) 2010-2013 Randy LeJeune" end-display
           display " " end-display.

       92000-abend.
           perform 30000-cleanup.

       93000-parse-cmdln.
           perform 90000-clear-screen
           if ws-cl-args = "-h" or "-H" or "--help" or "/h" or "/?"
               display "Usage: geekcode2.1 [options] file..."
               end-display
               display " " end-display
               display "Options: " end-display
               display "    -h, --help            Display this message"
               end-display
               display "    -v, --version         Display version"
               end-display
               perform 94000-terminate
           else if ws-cl-args = "-v" or "-V" or "--version"
               display "geekcode generator 0.3" end-display
               display "Copyright (C) 2010-2013 Randy LeJeune"
               display "EMail: <rflejeune@yahoo.com>"
               end-display
               display "License GPLv3+: GNU GPL version 3 or later - <ht
      -            "tp://gnu.org/licenses/gpl.html>."
               end-display
               display "This is free software: you are free to change an
      -            "d redistribute it."
               end-display
               display "There is NO WARRANTY, to the extent permitted by
      -            " law."
               end-display
               display " " end-display
               display " " end-display
               display "Written by Randy LeJeune." end-display
               perform 94000-terminate
           else if ws-cl-args = spaces
               continue
           else
               display "geekcode: invalid option.'" end-display
               display "Try `geekcode -h' for more information."
               end-display
               perform 94000-terminate
           end-if.

       94000-terminate.
           goback.
