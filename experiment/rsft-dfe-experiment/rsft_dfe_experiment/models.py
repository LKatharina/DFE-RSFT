from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random
import math
import numpy
import csv
from gettext import gettext as _
from . import exp
import itertools

author = 'Mathias Durrer, MatLaura Marbacher, Dr. Jana B. Jarecki'

doc = """
Risk sensitive foraging in decisions from experience format
"""


class Constants(BaseConstants):
  name_in_url = 'study' # how the experiment is called in the browser URL
  players_per_group = None # do we group players? Usually No
  num_familiarization_rounds = 1 # how many familiarization repetitions?
  num_repetitions = 1 # how many repetitions of stimuli?
  # num_samples = 3 # how many samples to draw in total
  num_samples_per_option = 3 #how manys amples per option
  num_trials = 3 # in the RSFT-choice, how many trials?
  num_oneshot = 0 # how many 1-choice trials?
  num_multitrial = num_repetitions + num_familiarization_rounds  
  num_rounds = int(exp.num_rounds) # exp.py calculates number of rounds
  initial_state = 0
  num_actions = 2
  duration = 55
  bonus_amount = 0.25
  # Below, language & labels that need translation
  lang = 'en'
  point_label = _('Punkte')
  trial_label = _('Entscheidung')
  action_label = _('Option')
  attention_fail_error = _("Nicht ganz richtig")

class Subsession(BaseSubsession):

  def concat_stimulus(self, i, stimuli):
    y = "_".join( str(x) for x in ['%.0f' % stimuli[i][0], '%.0f' % (stimuli[i][2] * 100), '%.0f' % stimuli[i][1]] )
    return(y)

  def creating_session(self):
    # possible conditions
    dfe_conditions = itertools.cycle([True, False])
    # Executed at the very start
    # Loops through all the round_numbers and players
    for p in self.get_players():
      if (self.round_number == 1):
        # Initialize the object 'Phasemanager' (see exp.py)
        p.participant.vars['PM'] = exp.Phasemanager(exp.phases, exp.stimuli, exp.blocks, exp.trials)
        # Initialize the object 'Appearancemanager' (see exp.py)
        p.participant.vars['AM'] = exp.Appearancemanager(p.participant.vars['PM'], exp.filepaths, exp.numfeatures, exp.numactions, exp.randomize_feature, exp.randomize_action, exp.randomize_stimulus_order)
      round_number = self.round_number
      phase_number = p.participant.vars['PM'].get_phaseN(round_number)
      phase = p.participant.vars['PM'].get_phaseL(round_number) # phase label
      stimuli = p.participant.vars['AM'].get_stimuli(round_number, phase_number)
      stimulus_position = p.participant.vars['AM'].get_action_position(round_number)
      feature_color = p.participant.vars['AM'].get_feature_appearance(round_number)[0]
      dfe_condition = next(dfe_conditions)
      p.participant.vars['dfe_condition'] = dfe_condition
      p.dfe_condition = dfe_condition

      # Store variables
      p.phase = p.participant.vars['PM'].get_phaseL(round_number)
      p.block = self.round_number
      p.budget = stimuli[2][0]
      p.stimulus0 = self.concat_stimulus(0, stimuli)
      p.stimulus1 = self.concat_stimulus(1, stimuli)
      p.state = stimuli[2][1]


      # this is only if we have one-shot trials in there
      # if (phase in ['critical']):
      #   p.trial = stimuli[2][2]
      # else:
      p.trial = 1
      p.successes = 0

      # Do and store the randomizations
      p.layout_featurecolor = '_'.join(''.join(str(y) for y in x) for x in [feature_color])
      p.layout_stimulusposition_01 = ''.join(str(x) for x in stimulus_position)

      # Initialize containers
      n = int(Constants.num_rounds + 1)
      if (self.round_number == 1):
        self.session.vars['instruction_rounds'] = p.participant.vars['PM'].get_instruction_rounds()
        p.participant.vars['bonus_rounds'] = p.participant.vars['PM'].get_bonus_rounds()
        p.participant.vars['stimulus_position'] = [None] * n
        p.participant.vars['img1'] = [None] * n
        p.participant.vars['img2'] = [None] * n
        p.participant.vars['max_earnings'] = [None] * n
        p.participant.vars['num_blocks'] = [None] * n
        p.participant.vars['decision_number'] = [None] * n
        p.participant.vars['outcomes'] = [None] * n
        p.participant.vars['sampling_outcomes'] = [None] * n
        p.participant.vars['block_number'] = [None] * n
         # goal_condition = 1 = goal shown during sampling; goal_condition = 0 = goal not shown during sampling #
        p.participant.vars['sample_condition'] = 1  # sample_condition = 0 = dfd (probabilities shown); sample_condition = 1 = dfe (probabilities not shown) #
        p.participant.vars['num_samples_per_option'] = Constants.num_samples_per_option
      p.participant.vars['stimulus_position'][round_number] = stimulus_position



      # Define the names of teh sprites for the images
      css_img_orig_position = [
        'sprite_' + p.stimulus0 + '_featurecolor' + p.layout_featurecolor,
        'sprite_' + p.stimulus1 + '_featurecolor' + p.layout_featurecolor
          ]
      p.participant.vars['img1'][round_number] = css_img_orig_position[stimulus_position[0]]
      p.participant.vars['img2'][round_number] = css_img_orig_position[stimulus_position[1]]
      if (phase in exp.phases):
        outcomes_orig_position = [p.draw_outcomes(x, Constants.num_trials) for x in p.participant.vars['AM'].get_stimuli(round_number, phase_number)[ :2]]
        p.participant.vars['outcomes'][round_number] = [outcomes_orig_position[i] for i in stimulus_position]
        sampling_outcomes_orig_position = [p.draw_outcomes(x, Constants.num_samples_per_option) for x in p.participant.vars['AM'].get_stimuli(round_number, phase_number)[ :2]]
        p.participant.vars['sampling_outcomes'][round_number] = [sampling_outcomes_orig_position[i] for i in stimulus_position]
      maxx = max([max(list(map(abs, stimuli[i]))) for i in [0,1]])
      max_earnings = max(maxx * (Constants.num_trials), p.budget)
      # select overall maximum points to be earned to determine the width of the progress bar
      if (round_number == 1):
        last_max_earnings = int(max_earnings)
      else:
        last_max_earnings = p.participant.vars['max_earnings'][(round_number - 1)]
      max_earnings = max(last_max_earnings, max_earnings)
      p.participant.vars['max_earnings'] = [max_earnings] * n
      p.participant.vars['num_blocks'][round_number] = p.participant.vars['PM'].get_num_trials_in_phase(round_number)
      p.participant.vars['decision_number'][round_number] = p.participant.vars['PM'].get_decision_number_in_phase(round_number)
      if (self.round_number == 1):
        p.successes = 0


class Group(BaseGroup):
  pass


# These functions 'make_..._field' generate the hidden html input fields
#   which will store the responses of participants
def make_choice_field(trial):
  return models.IntegerField(
    doc = "Chosen stimulus in trial" +str(trial) +", this is the stimulus, not the shown stimulus position")

def make_sample_field(trial):
  return models.IntegerField(
    blank=True,
    doc = "Sampled stimulus in trial" +str(trial) +", this is the stimulus, not the shown stimulus position")

def make_state_field(trial):
  return models.IntegerField(
    doc = "Point state at the beginning of trial" +str(trial))

def make_sample_state_field(trial):
  return models.IntegerField(
    blank=True,
    doc = "Point state at the beginning of trial" +str(trial))

def make_rt_field(trial):
  return models.FloatField(
    doc = "Reaction time  in ms from the end of the page load until the choice, in trial" +str(trial) +" or until submit, in case of instruction pages.")

def make_sample_rt_field(trial):
  return models.FloatField(
    blank=True,
    doc = "Reaction time  in ms from the end of the page load until the choice, in trial" +str(trial) +" or until submit, in case of instruction pages.")

# If sample.html and choices.html need different rt fields reactivate
#def make_sample_rt_field(trial):
#  return models.FloatField(
#    blank=True,
#    doc = "Reaction time  in ms from the end of the page load until the choice, in trial" +str(trial) +" or until submit, in case of instruction pages.")

# Every round the player object is re-initialized
class Player(BasePlayer):
  # Attention-Check Questions (q1 = ...) and Correct Answers (value != ...)

  # ENGLISH --------------------------------------------------------------

  #dfd
  g1e = models.IntegerField(
    label = "What is the number of the current decision?")
  def g1e_error_message(self, value):
    if value != 3:
      return Constants.attention_fail_error # error msg is defined above

  g2e = models.IntegerField(
    label = "How high is the threshold?")
  def g2e_error_message(self, value):
    if value != 29:
      return Constants.attention_fail_error

  g3e = models.IntegerField(label = "How high is the current point amount?")
  def g3e_error_message(self, value):
    if value != 12:
      return Constants.attention_fail_error

  g4e = models.IntegerField(label = "What is the maximum number of points that the right option offers?")
  def g4e_error_message(self, value):
    if value != 9:
      return Constants.attention_fail_error

  g5e = models.IntegerField(label="What is the probability to gain 3 points?")
  def g4e_error_message(self, value):
    if value != 60:
      return Constants.attention_fail_error

  # dfe
  l1e = models.IntegerField(
    label="What is the number of the current decision?")
  def g1e_error_message(self, value):
    if value != 3:
      return Constants.attention_fail_error  # error msg is defined above

  l2e = models.IntegerField(
    label="How high is the threshold?")
  def g2e_error_message(self, value):
    if value != 29:
      return Constants.attention_fail_error

  l3e = models.IntegerField(label="How high is the current point amount?")
  def g3e_error_message(self, value):
    if value != 12:
      return Constants.attention_fail_error

  l4e = models.IntegerField(label="What is the maximum number of points that the left option offers?")
  def g4e_error_message(self, value):
    if value != 9:
      return Constants.attention_fail_error

  a1e = models.IntegerField(
    choices=[
      [1, "I have understood this"],
      [2, "I have not understood this"]
    ],
    label= "Please indicate your answer",
    widget=widgets.RadioSelect
  )
  def a1e_error_message(self, value):
    if value != 1:
      return Constants.attention_fail_error

  # Coverstory 0
  c1e = models.IntegerField(
    widget=widgets.RadioSelect,
    label="In the learning phase of a block, your task is...",
    choices = [
      [1, "to meet or to exceed a threshold."],
      [2, "to learn about the hidden probabilities."],
      [3, "There is no task."]
    ])
  def c1_error_message(self, value):
    if value != 2:
      return Constants.attention_fail_error

  c2e = models.BooleanField(
    widget=widgets.RadioSelect,
    label= "Imagine you are in a choice phase, in which the threshold is 10. Your score in the end of a block is 10 points. This means you...",
    choices=[
      [True, "have reached the threshold."],
      [False, "have not reached the threshold."]
    ])
  def c2e_error_message(self, value):
    if value == False:
      return Constants.attention_fail_error

  c3e = models.BooleanField(
    widget=widgets.RadioSelect,
    label="In the choice phase of a block, your task is...",
    choices = [
      [1, "to meet or to exceed a threshold."],
      [2, "to learn about the hidden probabilities."],
      [3, "There is no task."]
    ])
  def c3e_error_message(self, value):
    if value != 1:
      return Constants.attention_fail_error

  c4e = models.IntegerField(
    widget=widgets.RadioSelect,
    label="In the learning phase of a block, there are...",
    choices=[
      [1, "50 trials."],
      [2, "5 trials."],
      [3, "50 trials, but you can move on any time"]
    ])
  def c4e_error_message(self, value):
    if value != 3:
      return Constants.attention_fail_error

  c5e = models.BooleanField(
    widget=widgets.RadioSelect,

    label="Imagine you are in the choice phase of a block and the threshold is 30 points. Your score in the end of a block is 28 points. This means you...",
    choices=[
      [False, "have reached the threshold."],
      [True, "have not reached the threshold."]
    ])
  def c5e_error_message(self, value):
    if value == False:
      return Constants.attention_fail_error

  c6e = models.IntegerField(
    widget=widgets.RadioSelect,
    label="Each amount of points occurs...",
    choices=[
      [1, "with a completely random probability."],
      [2, "with some probability."],
      [3, "There are no points."]
    ])
  def c6e_error_message(self, value):
    if value != 2:
      return Constants.attention_fail_error

  c7e = models.IntegerField(
    widget=widgets.RadioSelect,
    label="A Block is structured as follows...",
    choices=[
      [1, "First Choice phase, then learning phase (with 50 maximum choices for the whole experiment)."],
      [2, "First learning phase (with 50 maximum choices for each block), then choice phase."],
      [3, "There are no Blocks"]
    ])
  def c7e_error_message(self, value):
    if value != 2:
      return Constants.attention_fail_error


  #Incentives
  i4e = models.IntegerField(
    widget=widgets.RadioSelect,
    label="Which of the subsequent rounds are relevant for your bonus payment?",
    choices=[
      [1, "The first four rounds are relevant."],
      [2, "The last four rounds are relevant."],
      [3, "Each round is relevant, because four bonus rounds will be drawn randomly."]
    ])
  def i4_error_message(self, value):
    if value != 3:
      return Constants.attention_fail_error

  # GERMAN------------------------------------------------------------------
    # Gains
    g1 = models.IntegerField(
      label="Die wievielte Entscheidung treffen Sie gerade?")

    def g1_error_message(self, value):
      if value != 3:
        return Constants.attention_fail_error  # error msg is defined above

    g2 = models.IntegerField(
      label="Wie hoch ist der Schwellenwert?")

    def g2_error_message(self, value):
      if value != 0:
        return Constants.attention_fail_error

    g3 = models.IntegerField(label="Wie hoch ist Ihr Punktestand?")

    def g3_error_message(self, value):
      if value != -13:
        return Constants.attention_fail_error

    g4 = models.IntegerField(label="Wie viele Punkte ergibt die rechte Option maximal?")

    def g4_error_message(self, value):
      if value != 9:
        return Constants.attention_fail_error

    # Losses
    l1 = models.IntegerField(
      label="Wie hoch ist der Schwellenwert?")

    def l1_error_message(self, value):
      if value != 0:
        return Constants.attention_fail_error

    l2 = models.IntegerField(label="Wie hoch ist Ihr Punktestand?")

    def l2_error_message(self, value):
      if value != 11:
        return Constants.attention_fail_error

    l3 = models.IntegerField(
      label="Mit welcher Wahrscheinlichkeit ergibt die rechte Option -2 Punkte? (Zahl von 0-100)")

    def l3_error_message(self, value):
      if value != 60:
        return Constants.attention_fail_error

    l4 = models.IntegerField(
      label="Wie hoch ist der Startwert?")

    def l4_error_message(self, value):
      if value != 21:
        return Constants.attention_fail_error

    # Coverstory 0
    c1 = models.IntegerField(
      widget=widgets.RadioSelect,
      label="Ihr Ziel beim Punkte sammeln ist es ...",
      choices=[
        [1, "den Schwellenwert zu erreichen oder zu ??berschreiten."],
        [2, "den Schwellenwert nicht zu erreichen und nicht zu ??berschreiten."],
        [3, "Es gibt keine Zielvorgabe."]
      ])

    def c1_error_message(self, value):
      if value != 1:
        return Constants.attention_fail_error

    c2 = models.BooleanField(
      widget=widgets.RadioSelectHorizontal,
      label="Angenommen Sie sind beim Punkte sammeln und der Schwellenwert betr??gt 10 Punkte. Ihr Punktestand am Ende der Runde ist 10. Dann haben Sie ...",
      choices=[
        [True, "das Ziel erreicht."],
        [False, "das Ziel nicht erreicht."]
      ])

    def c2_error_message(self, value):
      if value == False:
        return Constants.attention_fail_error

    c3 = models.BooleanField(
      widget=widgets.RadioSelectHorizontal,
      label="Angenommen Sie sind beim Punkte sammeln und der Schwellenwert betr??gt 0 Punkte. Ihr Punktestand am Ende der Runde ist -2. Dann haben Sie ...",
      choices=[
        [False, "das Ziel erreicht."],
        [True, "das Ziel nicht erreicht."]
      ])

    def c3_error_message(self, value):
      if value == False:
        return Constants.attention_fail_error

    c4 = models.IntegerField(
      widget=widgets.RadioSelect,
      label="Ihr Ziel beim Punkte abgeben ist es...",
      choices=[
        [1, "den Schwellenwert zu erreichen oder zu unterschreiten."],
        [2, "den Schwellenwert nicht zu unterschreiten."],
        [3, "Es gibt keine Zielvorgabe."]
      ])

    def c4_error_message(self, value):
      if value != 2:
        return Constants.attention_fail_error

    c5 = models.BooleanField(
      widget=widgets.RadioSelectHorizontal,
      label="Angenommen Sie sind beim Punkte abgeben und der Schwellenwert betr??gt -10 Punkte. Ihr Punktestand am Ende der Runde ist -11. Dann haben Sie ...",
      choices=[
        [False, "das Ziel erreicht."],
        [True, "das Ziel nicht erreicht."]
      ])

    def c5_error_message(self, value):
      if value == False:
        return Constants.attention_fail_error

    c6 = models.BooleanField(
      widget=widgets.RadioSelectHorizontal,
      label="Angenommen Sie sind beim Punkte abgeben und der Schwellenwert betr??gt -10 Punkte. Ihr Punktestand am Ende der Runde ist -10. Dann haben Sie...",
      choices=[
        [True, "das Ziel erreicht."],
        [False, "das Ziel nicht erreicht."]
      ])

    def c6_error_message(self, value):
      if value == False:
        return Constants.attention_fail_error

    c7 = models.BooleanField(
      widget=widgets.RadioSelectHorizontal,
      label="Angenommen Sie sind beim Punkte abgeben und der Schwellenwert betr??gt 0 Punkte. Ihr Punktestand am Ende der Runde ist 3. Dann haben Sie...",
      choices=[
        [True, "das Ziel erreicht."],
        [False, "das Ziel nicht erreicht."]
      ])

    def c7_error_message(self, value):
      if value == False:
        return Constants.attention_fail_error

    # Incentives
    i1 = models.IntegerField(
      widget=widgets.RadioSelectHorizontal,
      label="... das Ziel in keiner der 5 relevanten Runden erreicht wurde?",
      choices=[0, 1, 2, 3, 4, 5, 6, 7, 8])

    def i1_error_message(self, value):
      if value != 1:
        return Constants.attention_fail_error  # error msg is defined above

    i2 = models.IntegerField(
      widget=widgets.RadioSelectHorizontal,
      label="... das Ziel in 5 von 5 relevanten Runden erreicht wurde?",
      choices=[0, 1, 2, 3, 4, 5, 6, 7, 8])

    def i2_error_message(self, value):
      if value != 6:
        return Constants.attention_fail_error

    i3 = models.IntegerField(
      widget=widgets.RadioSelectHorizontal,
      label="... das Ziel in 2 von 5 relevanten Runden erreicht wurde?",
      choices=[0, 1, 2, 3, 4, 5, 6, 7, 8])

    def i3_error_message(self, value):
      if value != 3:
        return Constants.attention_fail_error

    i4 = models.IntegerField(
      label="Welche der nachfolgenden Runden sind relevant f??r Ihren Gewinnchancen?",
      choices=[
        [1, "Die ersten 5 Runden sind relevant."],
        [2, "Die letzten 5 Runden sind relevant."],
        [3, "Jede Runde ist relevant: es werden f??nf Runden zuf??llig gezogen."]
      ])

    def i4_error_message(self, value):
      if value != 3:
        return Constants.attention_fail_error

  #
  # Variables needed in the experiment
  # --------------------------------------------------------------------------
  # prolificid = models.StringField(
  #  doc = "ID of the survey provider")
  browser = models.StringField(
    doc = "Browser and version", blank=True)
  phase = models.StringField(
    doc = "Phases during the experiment. Familiarization phase is not incentivized.")
  block = models.IntegerField(
    doc = "Current block")
  trial = models.FloatField(
    doc = "Current trial of 5")
  stimulus0 = models.StringField(
    doc = "Risky gamble number one, format x1_p1_x2.")
  stimulus1 = models.StringField(
    doc = "Risky gamble number two, format x1_p2_x2.")
  state = models.FloatField(
    doc = "Accumulated points before the current decision")
  budget = models.FloatField(
    doc = "Earnings requirement in current block")
  goal_condition = models.IntegerField(
    doc= "goal_condition: 1 is goal shown during sampling, 0 is goal hidden during sampling"
  )

  sample1 = make_sample_field(1)
  sample2 = make_sample_field(2)
  sample3 = make_sample_field(3)
  sample4 = make_sample_field(4)
  sample5 = make_sample_field(5)
  sample6 = make_sample_field(6)
  sample7 = make_sample_field(7)
  sample8 = make_sample_field(8)
  sample9 = make_sample_field(9)
  sample10 = make_sample_field(10)
  sample11 = make_sample_field(11)
  sample12 = make_sample_field(12)
  sample13 = make_sample_field(13)
  sample14 = make_sample_field(14)
  sample15 = make_sample_field(15)
  sample16 = make_sample_field(16)
  sample17 = make_sample_field(17)
  sample18 = make_sample_field(18)
  sample19 = make_sample_field(19)
  sample20 = make_sample_field(20)
  sample21 = make_sample_field(21)
  sample22 = make_sample_field(22)
  sample23 = make_sample_field(23)
  sample24 = make_sample_field(24)
  sample25 = make_sample_field(25)
  sample26 = make_sample_field(26)
  sample27 = make_sample_field(27)
  sample28 = make_sample_field(28)
  sample29 = make_sample_field(29)
  sample30 = make_sample_field(30)
  sample31 = make_sample_field(31)
  sample32 = make_sample_field(32)
  sample33 = make_sample_field(33)
  sample34 = make_sample_field(34)
  sample35 = make_sample_field(35)
  sample36 = make_sample_field(36)
  sample37 = make_sample_field(37)
  sample38 = make_sample_field(38)
  sample39 = make_sample_field(39)
  sample40 = make_sample_field(40)
  sample41 = make_sample_field(41)
  sample42 = make_sample_field(42)
  sample43 = make_sample_field(43)
  sample44 = make_sample_field(44)
  sample45 = make_sample_field(45)
  sample46 = make_sample_field(46)
  sample47 = make_sample_field(47)
  sample48 = make_sample_field(48)
  sample49 = make_sample_field(49)
  sample50 = make_sample_field(50)


  draw1 = make_sample_state_field(1)
  draw2 = make_sample_state_field(2)
  draw3 = make_sample_state_field(3)
  draw4 = make_sample_state_field(4)
  draw5 = make_sample_state_field(5)
  draw6 = make_sample_state_field(6)
  draw7 = make_sample_state_field(7)
  draw8 = make_sample_state_field(8)
  draw9 = make_sample_state_field(9)
  draw10 = make_sample_state_field(10)
  draw11 = make_sample_state_field(11)
  draw12 = make_sample_state_field(12)
  draw13 = make_sample_state_field(13)
  draw14 = make_sample_state_field(14)
  draw15 = make_sample_state_field(15)
  draw16 = make_sample_state_field(16)
  draw17 = make_sample_state_field(17)
  draw18 = make_sample_state_field(18)
  draw19 = make_sample_state_field(19)
  draw20 = make_sample_state_field(20)

  sample_rt_ms1 = make_sample_rt_field(1)
  sample_rt_ms2 = make_sample_rt_field(1)
  sample_rt_ms3 = make_sample_rt_field(1)
  sample_rt_ms4 = make_sample_rt_field(1)
  sample_rt_ms5 = make_sample_rt_field(1)
  sample_rt_ms6 = make_sample_rt_field(1)
  sample_rt_ms7 = make_sample_rt_field(1)
  sample_rt_ms8 = make_sample_rt_field(1)
  sample_rt_ms9 = make_sample_rt_field(1)
  sample_rt_ms10 = make_sample_rt_field(1)
  sample_rt_ms11 = make_sample_rt_field(1)
  sample_rt_ms12 = make_sample_rt_field(1)
  sample_rt_ms13 = make_sample_rt_field(1)
  sample_rt_ms14 = make_sample_rt_field(1)
  sample_rt_ms15 = make_sample_rt_field(1)
  sample_rt_ms16 = make_sample_rt_field(1)
  sample_rt_ms17 = make_sample_rt_field(1)
  sample_rt_ms18 = make_sample_rt_field(1)
  sample_rt_ms19 = make_sample_rt_field(1)
  sample_rt_ms20 = make_sample_rt_field(1)

  choice_safe1 = make_choice_field(1)
  choice_safe2 = make_choice_field(2)
  choice_safe3 = make_choice_field(3)

  state1  = make_state_field(1)
  state2  = make_state_field(2)
  state3  = make_state_field(3)
  state4  = make_state_field(4)


  rt_ms1 = make_rt_field(1)
  rt_ms2 = make_rt_field(1)
  rt_ms3 = make_rt_field(1)
  rt_ms4 = make_rt_field(1)
  rt_ms5 = make_rt_field(1)






  success = models.IntegerField(doc = "Indicator if in the current block the earnings requirement (budget) was reached, 1 if yes, 0 otherwise")
  # outcome = models.IntegerField(doc = "Randomly drawn outcome of the chosen option given the choice in this trial")
  successes = models.FloatField(initial = 0, doc = "Count of the total number of blocks where the earnings requirement (budget) was reached")
  rt_ms = models.FloatField(
    doc = "Reaction time from the end of the page load until the choice or until submit, in case of instruction pages.")
  layout_featurecolor = models.StringField(
    doc = "Layout: Randomized feature colors per trial (light vs dark grey), 01 means that in this trial feature x1 was light grey and feature x2 dark grey, 10 means that x1 was dark grey and x2 light grey.")
  layout_stimulusposition_01 = models.StringField(
    doc = "Layout: Randomized stimulus position per trial (left vs right). 01 means that stimulus1 was shown left, 10 means that stimulus1 was shown right.")


  def draw_outcomes(self, action, size):
    p = action[2: ][1]
    indices = numpy.random.binomial(n=1, p=p, size=size)
    #indices = [0, 1, 0, 1, 1, 0, 1, 0, 1, 1]
    x = action[ :2]
    res = [x[i] for i in indices]
    return res


  def get_last_state(self):
    if (self.round_number > 1):
      lastself = self.in_round(self.round_number - 1)
      return lastself.state + lastself.outcome
    else:
      return self.state

  def update_successes(self):
    n_ignore = Constants.num_familiarization_rounds
    successes = 0
    if (self.round_number >  n_ignore):
      successes = sum([p.success for p in self.in_rounds(n_ignore + 1, self.round_number - 1)])
      self.successes = successes
    return(successes)
    
  
  def get_last_success(self):
    round_num_ignore = 1 + Constants.num_familiarization_rounds
    if (self.round_number > round_num_ignore):
      return self.in_round(self.round_number - 1).success
    else:
      return 0


  # Variables that can be used in the html pages to show
  def vars_for_template(self):
    n = self.round_number
    return {
      'img1': self.participant.vars['img1'][n],
      'img2': self.participant.vars['img2'][n],
      'stimulus_position': self.participant.vars['stimulus_position'][n],
      'state': self.state,
      'budget': self.budget,
      'trial': self.trial,
      'max_earning': self.participant.vars['max_earnings'][n],
      'max_less_state': self.participant.vars['max_earnings'][n] - 0,
      'num_blocks': self.participant.vars['num_blocks'][n],
      'decision_number': self.participant.vars['decision_number'][n],
      'multitrial': self.phase in ['familiarization', 'training'],
      'dfe_condition': self.participant.vars['dfe_condition'],
      'num_samples': Constants.num_samples_per_option,
      'num_trials': Constants.num_trials
    }

  def draw_bonus(self):
    if self.round_number in self.participant.vars['bonus_rounds']:
      self.payoff = self.success


