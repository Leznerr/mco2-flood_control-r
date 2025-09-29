# validate.R
# ------------------------------------------------------------------------------
# Purpose : Schema/type/invariant checks for the DPWH flood-control CSV.
# Contract: validate_schema(df) -> invisible(TRUE) or stop() on violations

  dups <- nms[duplicated(nms)]
  if (length(dups) > 0L) {
    stop(sprintf("validate_schema(): duplicated column names: %s.", paste(sort(unique(dups)), collapse = ", ")))
  }


  required_strict <- c(
    "Region","MainIsland","Province","FundingYear","TypeOfWork",
    "StartDate","ActualCompletionDate","ApprovedBudgetForContract",
    "ContractCost","Contractor"
  )
  missing_strict <- setdiff(required_strict, nms)
  if (length(missing_strict) > 0L) {
    stop(sprintf("validate_schema(): missing required columns: %s.", paste(missing_strict, collapse = ", ")))
  }


  }

  invisible(TRUE)
}


  }
  invisible(TRUE)
}
