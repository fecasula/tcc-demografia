function Div(div)
  if div.identifier ~= "apresentacao" then
    return nil
  end

  local arquivo = "assets/apresentacao.pptx"
  local ppt = io.open(arquivo, "rb")

  if ppt then
    ppt:close()
    return pandoc.RawBlock("html", '<a class="acao acao-secundaria" href="' ..
      arquivo .. '"><i class="bi bi-easel" aria-hidden="true"></i> Apresentação (PPT)</a>')
  end

  return pandoc.RawBlock("html", [[
    <div class="apresentacao-pendente">
      <button class="acao acao-secundaria" type="button" disabled aria-describedby="estado-apresentacao">
        <i class="bi bi-easel" aria-hidden="true"></i> Apresentação (PPT)
      </button>
      <small id="estado-apresentacao">Em preparação</small>
    </div>
  ]])
end
