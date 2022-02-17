-- From https://pandoc.org/lua-filters.html#building-images-with-tikz
local system = require 'pandoc.system'

local tikz_doc_template = [[
\documentclass{standalone}
\usepackage{xcolor}
\definecolor{alum-1}{HTML}{EEEEEC}
\definecolor{alum-5}{HTML}{5F615C}
\definecolor{alum-6}{HTML}{2E3436}
\definecolor{cham-0}{HTML}{B4FA70}
\definecolor{cham-2}{HTML}{73D216}
\definecolor{cham-4}{HTML}{346604}
\definecolor{choc-1}{HTML}{E9B96E}
\definecolor{dkgreen}{rgb}{0,0.5,0}
\definecolor{dkred}{rgb}{0.5,0,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}
\definecolor{gt@blue}{RGB}{0,68,153}
\definecolor{gt@cyan}{RGB}{0,250,250}
\definecolor{gt@dkgray}{RGB}{98,110,128}
\definecolor{gt@dkred}{RGB}{128,0,0}
\definecolor{gt@gray}{RGB}{110,124,146}
\definecolor{gt@green}{RGB}{19,128,19}
\definecolor{gt@lightred}{RGB}{250,0,0}
\definecolor{gt@plum}{HTML}{75507b}
\definecolor{gt@red}{RGB}{216,30,5}
\definecolor{gt@yellow}{RGB}{255,128,0}
\definecolor{plum-3}{HTML}{5C3566}
\usepackage{pgfgantt}
\usepackage{tikz}
\usetikzlibrary{
    shapes
  , shapes.misc
  , shadows
  , trees
  , arrows
  , arrows.meta
  , fit
  , calc
  , positioning
  , backgrounds
  , decorations
  , decorations.pathmorphing
  , decorations.text
  , decorations.pathreplacing
  , patterns
  , matrix
  , shadows
  , fit
  , chains
}
\begin{document}
\nopagecolor
%s
\end{document}
]]

local function tikz2image(src, filetype, outfile)
  local f = io.open('tikz.tex', 'w')
  f:write(tikz_doc_template:format(src))
  f:close()
  os.execute('pdflatex tikz.tex')
  if filetype == 'pdf' then
    os.rename('tikz.pdf', outfile)
  else
    os.execute('pdf2svg tikz.pdf ' .. outfile)
  end
  os.remove('tikz.tex')
  os.remove('tikz.log')
  os.remove('tikz.aux')
  os.remove('tikz.pdf')
end

extension_for = {
  html = 'svg',
  html4 = 'svg',
  html5 = 'svg',
  latex = 'pdf',
  docx = 'pdf',
  beamer = 'pdf' }

local function file_exists(name)
  local f = io.open(name, 'r')
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

local function starts_with(start, str)
  return str:sub(1, #start) == start
end


function RawBlock(el)
 if (starts_with('\\begin{tikzpicture}', el.text) or
     starts_with('\\begin{ganttchart}', el.text)) then
    local filetype = extension_for[FORMAT] or 'svg'
    local fname = system.get_working_directory() .. '/' .. 'figures' .. '/' ..
        pandoc.sha1(el.text) .. '.' .. filetype
    local lname = 'figures/' .. pandoc.sha1(el.text) .. '.' .. filetype
    -- Ensure figures/ exists
    os.execute("mkdir -p figures/")
    if not file_exists(fname) then
      tikz2image(el.text, filetype, fname)
    end
    -- If there is a caption (e.g., in a LaTeX comment) then create a figure.
    local caption = string.match(el.text, "Caption: ([^\n]+)\n")
    local ref = string.match(el.text, "#fig:([^ \n]+)[ \n]")
    if caption then
       if ref then
          return pandoc.Para({pandoc.Image({pandoc.Str(caption)}, lname, "#fig:", "fig:"..ref)})
       else
          return pandoc.Para({pandoc.Image({pandoc.Str(caption)}, lname, "#fig:")})
       end
    else
       return pandoc.Para({pandoc.Image({}, lname)})
    end
  else
   return el
  end
end

-- pandoc --lua-filter tikz.lua -s -o cycle.html <<EOF
-- Here is a diagram of the cycle:
--
-- \begin{tikzpicture} %% Caption: This is an example figure.
-- \def \n {5}
-- \def \radius {3cm}
-- \def \margin {8} % margin in angles, depends on the radius
--
-- \foreach \s in {1,...,\n}
-- {
--   \node[draw, circle] at ({360/\n * (\s - 1)}:\radius) {$\s$};
--   \draw[->, >=latex] ({360/\n * (\s - 1)+\margin}:\radius)
--     arc ({360/\n * (\s - 1)+\margin}:{360/\n * (\s)-\margin}:\radius);
-- }
-- \end{tikzpicture}
-- EOF

-- Or with a figure reference via pandoc-fignos
-- (https://github.com/tomduck/pandoc-fignos):

-- pandoc --lua-filter tikz.lua --filter pandoc-fignos -s -o cycle.html <<EOF
-- Here is a diagram of the cycle shown in Figure @fig:cycle:
--
-- \begin{tikzpicture} %% Caption: This is an example figure.
--                     %% #fig:cycle
-- \def \n {5}
-- \def \radius {3cm}
-- \def \margin {8} % margin in angles, depends on the radius
--
-- \foreach \s in {1,...,\n}
-- {
--   \node[draw, circle] at ({360/\n * (\s - 1)}:\radius) {$\s$};
--   \draw[->, >=latex] ({360/\n * (\s - 1)+\margin}:\radius)
--     arc ({360/\n * (\s - 1)+\margin}:{360/\n * (\s)-\margin}:\radius);
-- }
-- \end{tikzpicture}
-- EOF
