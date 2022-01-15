#!/bin/bash
(
################################################################################
# setup
export PATH=/usr/bin:/bin
HERE=$(realpath $(dirname $0))
BASE=$(dirname $HERE)
export INTRINSICS=$BASE/intrinsics
export MANPATH=$BASE/man

# also need fpm(1)
export FPMPATH="$(dirname $(which fpm) )"

DOCS=$BASE/docs
MANDIR=$BASE/man

cd $BASE

export PATH="$HERE:$PATH"
export PATH="$BASE/bin:$PATH"
export PATH="$PATH:$FPMPATH"
################################################################################
# PURGE
cd $BASE ||exit
rm -frv man expected example
rm -fv src/M_intrinsics.f90
rm -fv docs/*
rm -fv bin/fman
rm -rfv build

mkdir -p $DOCS
mkdir -p $BASE/docs $BASE/example
mkdir -p $MANDIR/man3  $MANDIR/cat3  $MANDIR/man1
mkdir -p $BASE/expected
################################################################################
# FUNCTIONS
################################################################################
header(){
cat <<EOF
." Text automatically generated
.TH "$SHORTNAME" "3" "$(date +'%B %d, %Y')" "" "" " "
." -----------------------------------------------------------------
." * set default formatting
." disable hyphenation
.nh
." disable justification (adjust text to left margin only)
.ad l
." set smaller margin and spacing options
.ta T 0.2i
.nr IN 0.2i
." -----------------------------------------------------------------
EOF
}
################################################################################
TOCHARACTER(){
:
cat $NAME |
   sed -e "s/'/''/g" |
   sed -e "s/^/'/" |
   sed -e 's/$/'"'"', \&/'|
cat
}
################################################################################
# rebuild man-pages and M_intrinsics.f90
cd $INTRINSICS
for NAME in *.md
do
   case "$NAME" in
   *_index.md) ;;
   index.md) ;;
   XXGNU_Free_Documentation_License.md) ;;
   *)
      SHORTNAME=$(basename $NAME .md)
      SHORTNAME=${SHORTNAME,,}
      echo "NAME: $NAME to $SHORTNAME"
      ######################################################
      # man-pages
      (
         header
         sed -n -e '\%^##%,${ p }'  $NAME |
            pandoc -f commonmark -t man --columns=72
      )>$BASE/man/man3/$SHORTNAME.3fortran
      sed -i -r 's/(\.SS \\f\[B\])(.*)(\\f.*)/\1\U\2\E\3/' $BASE/man/man3/$SHORTNAME.3fortran
      sed -i -r 's/(\.SS \\f\[B\])(.*)(\\f.*)/.SH \U\2/' $BASE/man/man3/$SHORTNAME.3fortran
      gzip --force $BASE/man/man3/$SHORTNAME.3fortran
      ######################################################
      (
         sed -n -e '\%^##%,${ p }'  $NAME |
            pandoc -f markdown_mmd -t plain --columns=72 |
	    # remove trailing whitespace
	    sed -e 's/ *$//' |
	    # delete blank lines at top of file
	    awk 'NF {f=1} f' |
	    # get rid of artifact from converting from markdown
	    grep -v '^-$' |
	    cat -s
      )>$BASE/man/cat3/$SHORTNAME.3fortran
   ;;
   esac
done

# removes the other directories ??????????
cd $BASE/man
mandb -c .
env MANWIDTH=256 man --manpath=$BASE/man -k .|col -b
env MANWIDTH=80  man --manpath=$BASE/man --regex '.*'|
    col -b |
    expand --tabs=3 
################################################################################
cd $BASE 
tar cvfz docs/fortran.tgz man
################################################################################
export NAME

cd $BASE/man/cat3
ls *.3fortran|while read NAME
do
   SHORTNAME=${NAME/.*}
   (
   cat <<EOF
   function help_${SHORTNAME}(prefix,topic,m_help) result (textblock)
   character(len=256),allocatable   :: textblock(:)
   logical,intent(in),optional      :: prefix
   logical,intent(in),optional      :: topic
   logical,intent(in),optional      :: m_help
   character(len=*),parameter       :: shortname="$SHORTNAME"
   character(len=:),allocatable,intent(out),optional :: name
   textblock=[character(len=256)    :: &
   '', &
   $(TOCHARACTER)
   '']
      if(present(topic))then
         textblock=[shortname]
      elseif(present(prefix))then
         if(prefix)then
            do i=1,size(textblock)
               textblock(i)=shortname//':'//trim(textblock(i))
            enddo
         endif
      elseif(present(m_help))then
         if(m_help)then
            textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname in
   	 textblock=' '//textblock ! shift to right by one character 
            textblock(1)=shortname
         endif
      endif
   end function help_${SHORTNAME}
EOF
   )
done 
################################################################################
cd $BASE/man/cat3
cat <<\EOF
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_intrinsics
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)                       :: name
logical,intent(in),optional                       :: prefix
logical,intent(in),optional                       :: topic
logical,intent(in),optional                       :: m_help
character(len=256),allocatable                    :: textblock(:)
character(len=:),allocatable                      :: a, b, c
integer                                           :: i, p, pg
   select case(name)
   case('','manual','intrinsics','fortranmanual','fortran_manual')
      textblock=help_intrinsics_all(prefix,topic,m_help)
   case('fortran','toc')
      textblock=help_intrinsics_section()
      do i=1,size(textblock)
         p = index(textblock(i), '[')
         pg = index(textblock(i), ']')
         if(p.gt.0.and.pg.gt.p)then
          a=textblock(i)(:p-1)
          b=textblock(i)(p:pg)
          c=textblock(i)(pg+1:)
          textblock(i)=b//' '//a//c
         endif
      enddo
      call sort_name(textblock)
   case default
      textblock=help_intrinsics_one(name,prefix,topic,m_help)
   end select
end function help_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_section() result (textblock)

!@(#) grab lines in NAME section and append them to generate an index of manpages

character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: add(:)
character(len=256),allocatable  :: label
character(len=10)               :: cnum
integer                         :: i
integer                         :: icount
logical                         :: is_label
logical                         :: grab
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum)
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      do i=1,size(add)
         if(add(i).ne.'')then
            is_label=verify(add(i)(1:1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0 &
            .and. verify(trim(add(i)),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0
         endif
         if(add(i).eq.'')then
            ! skip
         elseif(is_label.and.add(i).eq.'NAME')then
            grab=.true.
         elseif(is_label)then
            exit
         elseif(grab)then
            label=adjustl(trim(label))//' '//adjustl(compact(trim(add(i))))
         endif
      enddo
      textblock=[character(len=256) :: textblock,compact(label)]
      icount=icount + 1
   enddo
end function help_intrinsics_section
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_all(prefix,topic,m_help) result (textblock)
logical,intent(in),optional     :: prefix
logical,intent(in),optional     :: topic
logical,intent(in),optional     :: m_help
character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: header(:)
character(len=256),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum,prefix,topic,m_help)
      if( size(add) .eq. 0 ) exit
      textblock=[character(len=256) :: textblock,add]
      icount=icount + 1
   enddo
   if(present(m_help))then
      if(m_help)then
         header=[ character(len=256) :: &
         '================================================================================',    &
         'SUMMARY',    &
         ' The primary Fortran topics are',    &
         ' tan                   tanh                      this_image',    &
         '']
         textblock=[header,textblock]
      endif
   endif
end function help_intrinsics_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_one(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: prefix
logical,intent(in),optional      :: m_help
logical,intent(in),optional      :: topic
character(len=256),allocatable   :: textblock(:)
character(len=:),allocatable     :: shortname
integer                          :: i
select case(name)
EOF

COUNT=0
ls *.3fortran|while read NAME
do
   SHORTNAME=${NAME/.*}
   COUNT=$((COUNT+1))
   cat <<EOF

case('$COUNT','$SHORTNAME')

textblock=[character(len=256) :: &
'', &
$(TOCHARACTER)
'']

shortname="$SHORTNAME"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif

EOF
done
   cat <<\EOF
case default
   allocate (character(len=256) :: textblock(0))
end select
end function help_intrinsics_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_name(lines)
!@(#) sort_name(3fp):sort strings(a-z) over specified field using shell sort starting with [ character
character(len = *)                :: lines(:)
   character(len = :),allocatable :: ihold
   integer                        :: n, igap, i, j, k, jg
   n = size(lines)
   if(n.gt.0)then
      allocate(character(len = len(lines(1))) :: ihold)
   else
      ihold = ''
   endif
   igap = n
   INFINITE: do
      igap = igap/2
      if(igap.eq.0) exit INFINITE
      k = n-igap
      i = 1
      INNER: do
         j = i
         INSIDE: do
            jg = j+igap
            if( lle( lower(lines(j)), lower(lines(jg)) ) )exit INSIDE
            ihold = lines(j)
            lines(j) = lines(jg)
            lines(jg) = ihold
            j = j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i = i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)
!@(#) M_strings::lower(3f): Changes a string to lowercase over specified range
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)     ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

!$@(#) M_strings::compact(3f): Converts white-space to single spaces; removes leading spaces

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
EOF
################################################################################
# make HTML slides page

cd $INTRINSICS
FILES=$(
   for NAME in *.md
   do
   case "$NAME" in
   index.md);;
   *_index.md);;
   GNU*);;
   *)echo "$NAME" ;;
   esac
   done
)
echo FILES $FILES|xargs -n 5|column -t

echo "creating $BASE/docs/intrinsics.md" 1>&2
for NAME in $FILES
do
   echo "# $( basename ${NAME} .md)"|sed -e 's/_/\\_/g'
   # bug when label contains underscore
   #echo "# $( basename ${NAME} .md)"|sed -e 's/_//g'
   # bug in some conversions if no blank line above a section
   echo ""

   sed -n -e '\%^##%,${ p }'  $NAME |
   # remove trailing whitespace
   sed -e 's/ *$//' |
   # delete blank lines at top of file
   awk 'NF {f=1} f' |
   # get rid of artifact from converting from markdown
   grep -v '^-$' |
   expand |
   cat -s
done >$BASE/docs/intrinsics.md

echo "creating $BASE/docs/intrinsics_slidy.html" 1>&2
pandoc -t slidy  --metadata title="Fortran Intrinsics" -s "$BASE/docs/intrinsics.md" \
   --standalone --slide-level=1 -o $BASE/docs/intrinsics_slidy.html

################################################################################
# build new version of fman

# some expected problems for now
cd $BASE ||exit
mv example/c_f_pointer.f90 example/c_f_procpointer.f90 example/c_funloc.f90 expected

fpm build 
fpm install --prefix $BASE

(
   # extract demo programs
   cd $BASE/intrinsics
   for NAME in *.md
   do
   case "$NAME" in
   index.md|*_index.md);;
   GNU_Free_Documentation_License.md);;
   *)
      TOPIC=$(basename ${NAME,,} .md)
      echo $NAME $TOPIC
      fman -d $TOPIC > $BASE/example/$TOPIC.f90
   ;;
   esac
   done
)

# build new version of demo programs
PROBLEMS
fpm build 

# build fpm documenation from help too
#################################################
#@(#) create man-pages, markdown and html slidy(1) files of fpm(1) help text using txt2man(1) and pandoc(1)
# liked results better tnan from txt to man-pages using pandoc
# can use groff to turn man-pages into a lot of formats as well
###############################################################################
cat >$DOCS/slidy.md <<\EOF
# FPM
## Fortran Package Manager
EOF
(
for NAME in fpm new build run test runner install update list help version
do
   echo "# $NAME"
   echo " "
   fpm help $NAME|
   txt2man | 
   tee $MANDIR/man1/$NAME.1 |
   pandoc -t gfm -f man --columns=72 |
   sed -e 's/fpm: *Leaving directory.*//' |
   sed -e 's@/\*\*fpm\*\*@/fpm@g' >/$DOCS/$NAME.md
   cat $DOCS/$NAME.md |
   sed -e 's/^>//'|
   sed -e 's/\(^# \)\([A-Z][A-Z ]*\)/\1__\L\2__/'|
   sed -e 's/^# /## /'
done
) >>$DOCS/fpm_slidy.md
###############################################################################
pandoc -f gfm \
 --columns=72 \
 --slide-level=1 \
 --metadata pagetitle="FPM command" \
 --self-contained \
 --standalone -o $DOCS/fpm_slidy.html \
 -t slidy \
 $DOCS/fpm_slidy.md
###############################################################################
echo 'see docs/ and man/ directories for output'
###############################################################################
#fman manual|spell
#fman manual|findll -l 80
###############################################################################
cd $BASE
git add .
git commit -m 'update docs'
git checkout main
cp intrinsics/*.md $HOME/github/FORK/fortran-lang.org/learn/intrinsics/
###############################################################################
) |tee /tmp/all.log
###############################################################################
exit
###############################################################################
