try
    using Revise
catch
    println("Revise.jl not loaded")
end

const JULIA_LOCAL_PACKAGES = expanduser("~/src/")
push!(LOAD_PATH, JULIA_LOCAL_PACKAGES)

ENV["EDITOR"] = "emacsclient"
ENV["JULIA_LOCAL_PACKAGES"] = JULIA_LOCAL_PACKAGES
