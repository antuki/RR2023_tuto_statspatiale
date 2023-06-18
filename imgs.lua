function Header(el)
  if not el.attributes['background-image'] then
    el.attributes['background-image'] = 'figures/blackboard.png'
    el.attributes['background-size'] = 'cover'
  end
  
  return el
end
