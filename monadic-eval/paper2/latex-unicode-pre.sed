s/«/\$/g
s/»/\$/g
s/⸤/_{/g
s/⸥/}/g
s/⸢/^{/g
s/⸣/}/g
s/⦑/\\text{/g
s/⦒/}/g
s/⟬/\\texttt{/g
s/⟭/}/g
s/∣/\\mathrel{\|}/g
s/⋆/\^\*/g
s/∿/\\widetilde/g
s/♯/\\widehat/g
s/⇄/\\galois/g
s/␣/\\texttt{\\hspace{1em}}/g

s/ℑ⁅/\\\\\\begin{minipage}{\\linewidth}\\begin{lstlisting}/g
s/ℑ,/\\end{lstlisting}\\resultskip\\begin{lstlisting}\[style=result\]/g
s/ℑ;/\\end{lstlisting}\\resultskip\\begin{lstlisting}/g
s/ℑ⁆/\\end{lstlisting}\\end{minipage}/g

:L1
s/⸨\([^⸩]*[^‹]\)\{0,1\}\([λδσςρφθΣ∅←≔₀₁₂′¢∈×⊥¬]\)\([^⸩]*\)⸩/⸨\1‹\2›\3⸩/g
tL1
s/𝔥⸨/\\hbox{\\lstinline|/g
s/⸨/{\\lstinline|/g
s/⸩/|}/g
:L2
s/¦ \(.*[^‹]\)\{0,1\}\([λδσςρφθΣ∅←≔₀₁₂′¢∈×⊥¬]\)\(.*\)$/¦ \1‹\2›\3/g
tL2
s/¦ //g
