signature HOOK_GC_STRUCTS =
sig
    include SSA_TRANSFORM
end

signature HOOK_GC =
sig
    include HOOK_GC_STRUCTS
end
