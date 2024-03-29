% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_r-objects.R
\docType{data}
\name{repository_files}
\alias{repository_files}
\title{Repository Files}
\format{
An object of class \code{list} of length 7.
}
\usage{
repository_files
}
\description{
A list of names and tests with which to refer to the \code{LabOrga}
repository files.
}
\details{
Currently, to be a valid \code{LabOrga} repository the folder must contain
7 files.

**1. dlevel1_tissue_donor.RDS **
The tissue donor table is a data.frame in which each row corresponds to a tissue
donor from whom tissue samples have been extracted. It must contain the variables
\emph{institution}, \emph{donor_species} and \emph{donor_tag}.

**2. dlevel2_tissue_sample.RDS **
The tissue sample table is a data.frame in which each row corresponds to a
tissue sample, a contiguous tissue sample or, in case of liquids, a tube.
In addition to the ID variables from its tissue donor (merged in \emph{id_tissue_donor_num}),
with which tissue samples inherit all information of their respective tissue donor, the
the table must contain the ID variables \emph{date_of_extraction} and \emph{sample_index}.

**3. dlevel3_tissue_portion.RDS **
The tissue portion table is a data.frame in which each row corresponds to
a tissue portion, the unit in which tissue is stored in the lab after processing
the tissue sample it derived from. In addition to the ID variables from its
tissue sample (merged in \emph{id_tissue_sample_num}), with which tissue portions
inherit all information of their respective tissue sample, the table must contain
the ID variable \emph{portion_index}.

** 4. dlevel4_raw_data.RDS **
The raw data table is a data.frame in which each row corresponds to the data output
of an assay/experiment that was conducted with a tissue portion. In addition to the
ID variables from the tissue portion with which the data was created (merged in
\emph{id_tissue_portion_num}), the table must contain the ID variable \emph{raw_data_index} as
well as the variables \emph{link_raw_data} and \emph{assay_trademark}.

** 5. logfile.RDS **
The logfile is data.frame in which each row corresponds to a change made in the
repository during a session within the \code{LabOrga} interface. This includes,
adding, edditing and deleting data entries. It must contain the variables \emph{username},
\emph{fn_name} and \emph{entry_id}.

** 6. setup.RDS **
The setup file is a list in which information regarding the repository set up
are stored.

\itemize{
\item{\emph{connected_with}:}{ A list of devices suffixed with an index. Each slot of this list corresponds to the output
of \code{Sys.info()} for the device that was connected to the repository.}
\item{\emph{created_by}:}{ The output of \code{Sys.info()} fo rthe device that created the repository.}
\item{\emph{created_at}:}{ The date time when the repository was created.}
\item{\emph{version}:}{ A list of three slots named \emph{major}, \emph{minor} and \emph{patch}. Used to keep track
if the installed version of \code{LabOrga} corresponds to the version of the repository.}
}

** 7. users.RDS**
A data.frame in which each row corresponds to a user. In addition
to the name (\emph{username}) the variables \emph{password} and \emph{permission}
must exist.
}
\seealso{
\link{data_variables}
}
\keyword{datasets}
